;;; yololisp-compiler.el --- YOLOLISP compiler  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; Some sort of YOLOL Lisp-compiler...
;;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'subr-x))

(defvar *yl-separator* ""
  "Output YOLOL's operator/operand output separator.
Set this to \" \" for more readable YOLOL output.")

(defun yl-join-exprs (&rest exprs)
  "Join expressions `EXPRS' with *YL-SEPARATOR*."
  (string-join exprs *yl-separator*))

(defvar yl-op-precedences
  (eval-when-compile
    (thread-last '(== != ^ / * + -)
      (nreverse)
      (seq-map-indexed #'cons)))
  "Alist mapping operators to operator-precedences.")

(defun yl-get-precedence (op)
  "Look up precedence for operator `OP'."
  (cdr (assoc op yl-op-precedences)))

(defun yl-try-parenthesize (last-precedence op expr)
  "Attempt to parenthesize expression `EXPR'.
This only occurs iff `OP's precedence is lower than
`LAST-PRECEDENCE'."
  (let ((op-precedence (yl-get-precedence op)))
    (if (< op-precedence last-precedence)
        (concat "(" expr ")")
      expr)))

(defvar yl-unary-left
  '((neg . "-")
    (sqrt . "sqrt ")
    (abs . "abs ")
    (not . "not ")
    (! . "!")
    (preinc . "++")
    (predec . "--")
    ;; trig
    (sin . "sin ")
    (cos . "cos ")
    (tan . "tan ")
    (asin . "asin ")
    (acos . "acos ")
    (atan . "atan "))
  "Unary operators on left-side of value.")

(defvar yl-unary-right
  '((inc . "++")
    (dec . "--")
    (fact . "!"))
  "Unary operators on right-side of value.")

(cl-defun yl-compile-expr (expr &optional (last-precedence 0))
  "Compiles a YOLOLISP expression `EXPR'.
`LAST-PRECEDENCE' is the precedence of the parent operation; if
this is a higher precedence operation then parentheses won't be
added.  0 is the seed precedence since top level exprs dont't
need parens."
  (cl-etypecase expr
    (cons (let ((result (let* ((op (car expr))
                               (arg1 (cadr expr))
                               (arg2 (caddr expr))
                               (left-one-arg-op (cdr (assoc op yl-unary-left)))
                               (right-one-arg-op (cdr (assoc op yl-unary-right))))
                          (cond
                           ;; one argument operation, value on left
                           (left-one-arg-op
                            (concat left-one-arg-op (yl-compile-expr arg1)))
                           ;; one argument operation, value on right
                           (right-one-arg-op
                            (concat (yl-compile-expr arg1) right-one-arg-op))
                           ;; two-arg binary operation
                           (t
                            (let ((op-precedence (yl-get-precedence op)))
                              (yl-try-parenthesize
                               last-precedence op
                               (yl-join-exprs (yl-compile-expr arg1 op-precedence)
                                              (symbol-name op)
                                              (yl-compile-expr arg2 op-precedence))))))
                          )))
            result))
    (symbol  (symbol-name expr))
    (string  (replace-regexp-in-string "\n" "\\\\n" (concat "\"" expr "\"")))
    (integer (number-to-string expr))
    (float   (number-to-string expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YOLOL PRIMITIVE COMPILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some nomenclature: a "fragment" of YOLOL is a string of output YOLOL like "a
;; = 10" or "if a = 1 then b = 1 end". The compiler outputs lists of these YOLOL
;; fragments, which can then be rearranged to meet chip code size limits.

(defun yl-join-fragments (forms &optional separator)
  "Joins the compilation result (`FORMS') of YOLOLISP's DO form.

Accepts an optional `SEPARATOR' string."
  ;; FIXME: FLATTEN-TREE may not be available in older Emacs versions
  (string-join (flatten-tree forms) (or separator " ")))

(defun yl-compile-if (condition tbranch &optional fbranch)
  "Compile IF with condition `CONDITION' and truth-branch `TBRANCH'.
Optionally supply a false-branch `FBRANCH'."
  ;; Branches are compiled with YL-COMPILE-FORM, and the resulting fragments are
  ;; joined with YL-JOIN-FRAGMENTS. We join because YOLOL's IF constrains the
  ;; branch code to a single line, so we have to treat IF as a single
  ;; "fragment", even though the branch code consists of viable fragments.
  ;; (...in future we ought to retain the fragments for analysis reasons)
  (if fbranch
      (concat "if "
              (yl-compile-expr condition)
              " then "
              (yl-join-fragments (yl-compile-form tbranch))
              " else "
              (yl-join-fragments (yl-compile-form fbranch))
              " end")
    (concat "if "
            (yl-compile-expr condition)
            " then "
            (yl-join-fragments (yl-compile-form tbranch))
            " end")))

(defun yl-compile-assign (var expr)
  "Compile ASSIGN of `EXPR' to `VAR'."
  (yl-join-exprs (symbol-name var)
                 "="
                 (yl-compile-expr expr)))

(defun yl-compile-op-assign (var op expr)
  "Compile OP-ASSIGN, with `OP', of `EXPR' to `VAR'."
  (cl-ecase op
    ((+ - * /)
     (yl-join-exprs (symbol-name var)
                    (format "%s=" op)
                    (yl-compile-expr expr)))))

(defun yl-compile-goto (expr)
  "Compile GOTO `EXPR'."
  (concat "goto " (yl-compile-expr expr)))

(defun yl-compile-do (forms)
  "Compile DO form consisting of `FORMS'.
Drops any found DECLARE forms while keeping the declaration
environment intact."
  (with-decl-env (forms)
    (if (eq 'declare (caar forms))
        (mapcar #'yl-compile-form (cdr forms))
      (mapcar #'yl-compile-form forms))))

(defun yl-compile-label ()
  "Compile LABEL - we don't do this (yet?)."
  "")

(defun yl-compile-form (form)
  "Compile a YOLOLISP form `FORM' into YOLOL fragments.
Returns a list of lists of fragments."
  (let ((sym (car form)))
    (cl-ecase sym
      (do     (yl-compile-do (cdr form)))
      (if     (list (yl-compile-if (cadr form) (caddr form) (cadddr form))))
      (assign (list (yl-compile-assign (cadr form) (caddr form))))
      (op-assign (list (yl-compile-op-assign (cadr form) (caddr form) (cadddr form))))
      (label  (list (yl-compile-label)))
      (goto   (list (yl-compile-goto (cadr form))))
      (literal (list (cadr form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YOLOLISP-MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: ...could we define these in YOLOLISP?
;;       ...should we?
;;       ...it's very un-Lispy to _not_ allow user-defined macros!

(defun yl-macro-when (form)
  `(if ,(cadr form) ,(caddr form)))

(defun yl-macro-unless (form)
  `(if (! ,(cadr form)) ,(caddr form)))

(defun yl-macro-// (form)
  `(literal ,(concat "//" (cadr form))))

(defun yl-transform-assign-pair (pair)
  (let* ((var    (car pair))
         (rvalue (cadr pair))
         ;; Pull out a potential assign operator
         (assign-op (when (and (consp rvalue) (eq var (cadr rvalue)))
                      (car rvalue))))
    (if assign-op
        (list 'op-assign var assign-op (caddr rvalue))
      (list 'assign var rvalue))))

(defun yl-macro-set (form)
  (let* ((pairs (seq-partition (cdr form) 2))
         (assign-pairs (mapcar #'yl-transform-assign-pair pairs)))
    `(do ,@assign-pairs)))

(defun yl-macro-comment-line-length (form)
  `(// " <-------------- this line is 70 characters long ------------------>"))

(defvar yl-macro-registry
  '((when   . yl-macro-when)
    (unless . yl-macro-unless)
    (set    . yl-macro-set)
    (//     . yl-macro-//)
    (//-line-length . yl-macro-comment-line-length)))

(defun yl-get-macro (symbol)
  (assoc symbol yl-macro-registry))

(defun yl-expand-macro (form)
  (funcall (cdr (yl-get-macro (car form))) form))

(defun yl-macroexpand (form)
  "Recursively and repeatedly expand all macros in `FORM'."
  (if-let ((sym (and (consp form) (car form))))
      (if (yl-get-macro sym)
          (yl-macroexpand (yl-expand-macro form))
        (cl-ecase sym
          (do        `(do ,@(mapcar #'yl-macroexpand (cdr form))))
          (assign    `(assign ,(cadr form) ,(yl-macroexpand (caddr form))))
          (op-assign `(op-assign ,(cadr form) ,(caddr form) ,(yl-macroexpand (cadddr form))))
          (goto      `(goto ,(yl-macroexpand (cadr form))))
          (if        `(if ,(yl-macroexpand (cadr form))
                          ,(yl-macroexpand (caddr form))
                        ;; conditionally splice in the fbranch if it exists
                        ,@(when-let ((fbranch (yl-macroexpand (cadddr form))))
                            (list fbranch))))
          (otherwise form)))
    form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DO-COLLAPSER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collapse-do-forms (form)
  "Collapse 'simple' nested DO forms in `FORM'.
Doesn't attempt to collapse DOs with an associated DECLARE."
  (mapcan (lambda (f)
            (if (and (consp f)
                     (eq 'do (car f)))
                ;; NOTE: we don't collapse DOs with DECLAREs into parents.
                ;;       DECLARE adds additional lexical information that
                ;;       would be otherwise be lost.
                (if (and (consp (cadr f))
                         (eq 'declare (caadr f)))
                    (list (collapse-do-forms f))
                  (collapse-do-forms (cdr f)))
              (list f)))
          form))

(defun yl-do-collapser (form)
  "Collapse simple DO blocks within `FORM'."
  (if-let ((sym (and (consp form) (car form))))
      (cl-ecase sym
        (do `(do ,@(collapse-do-forms (cdr form))))
        (otherwise form))
    form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE SIZE CONSTRAINER / FRAGMENT REARRANGER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum-list (list)
  "Sums all numbers in `LIST'."
  (seq-reduce #'+ list 0))

(defun toplevel-do-rearranger (form &optional column-width)
  "Rearrange a top level DO form `FORM' to a YOLOL-esque sexp format.
The format is line-and-column-conscious and very closely
resembles the eventual YOLOL output, rearranging forms across
lines based on their expected compiled YOLOL output.

`COLUMN-WIDTH' (default: 70)"
  ;;
  ;; The main motivation for this YOLOL-analogue format is LABEL and GOTO
  ;; referencing: LABEL is sensitive to eventual output position, and meanwhile
  ;; GOTOs need to be able to translate a label into a line number.
  ;;
  ;; Code adapted from the original string fragment rearranger.
  ;;
  ;; TODO: RENAME/REFACTOR/REDESCRIBE THIS GARBAGE.
  ;;
  ;; TODO: This function repeatedly calls YL-COMPILE-FORM to figure out the
  ;; lengths of the output YOLOL fragments. We should memoize this call.
  ;;
  (when (eq 'do (car form))
    (cl-loop
     with forms = (cdr form)
     ;; CONSTRAINED-FORM-LISTS is a list of lists of forms that are
     ;; column-constrained. Each entry in this list corresponds to a line of
     ;; output YOLOL.
     with constrained-form-lists = (list nil)

     ;; Loop over all of our forms.
     for current-form in forms

     ;; The current output line's forms
     for current-line-forms = (car constrained-form-lists)

     ;; sum current line's form's compiler output length, and factor in spacing
     ;; to get total line's length (the final fragment has no space appended,
     ;; hence the 1-)
     ;; TODO: fix ugly
     for total-line-length = (+ (sum-list
                                 (mapcar (lambda (f)
                                           (sum-list
                                            (mapcar #'length
                                                    (flatten-tree (yl-compile-form f)))))
                                         current-line-forms))
                                (1- (length current-line-forms)))
     for current-form-length = (sum-list (mapcar #'length (flatten-tree (yl-compile-form current-form))))

     ;; when we exceed the current line's column limit, create a new line, and
     ;; repoint to it
     when (>= (+ total-line-length current-form-length) (or column-width 70))
     do
     (push nil constrained-form-lists)

     ;; and finally, add the form to the appropriate line
     do
     (push current-form (car constrained-form-lists))

     ;; correct the ordering of all of these pushes by reversing everything
     finally return (nreverse (mapcar #'nreverse constrained-form-lists)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MULTIPASS OPTIMIZER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TYPE TAGGER PASS
;;  * Uses declaration env (DECLARE) to tag expressions with types
;;  * Type tags will be used in later optimisations
;;
;; TYPE-TAG STRIP PASS
;;  * Removes all type tags from code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *declarations* nil
  "The declaration environment.")

(defun yl-get-declarations (kind)
  (mapcar #'cdr
          (mapcan
           (lambda (decls)
             (seq-filter (lambda (spec) (eq (car spec) kind)) decls))
           *declarations*)))

(cl-defmacro with-decl-env ((forms) &body body)
  (declare (indent 1))
  `(let ((*declarations*
          (or (and (eq 'declare (caar forms))
                   (cons (cdar forms) *declarations*))
              *declarations*)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPTIMIZER: TYPE TAGGER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-lookup-var-type (sym)
  "Look up the type of `SYM' in the declaration environment."
  (let* ((type-decls (yl-get-declarations 'type))
         (var-lookup (mapcan
                      (lambda (entry)
                        (let ((type (car entry))
                              (vars (cdr entry)))
                          (mapcar (apply-partially #'cons type) vars)))
                      type-decls)))
    (car (rassoc sym var-lookup))))

(defun yl-try-type-tag (sym)
  (if-let ((var-type (yl-lookup-var-type sym)))
      (list var-type sym)
    (if (integerp sym)
        `(integer ,sym)
      sym)))

(defun yl-type-tagger-optimize-do (forms)
  (with-decl-env (forms)
    (mapcar #'yl-type-tagger-optimize forms)))

(defun yl-type-tagger-optimize (form)
  (let ((sym (car form)))
    (cl-case sym
      (do        `(do ,@(yl-type-tagger-optimize-do (cdr form))))
      (assign    `(assign ,(cadr form) ,(yl-try-type-tag (caddr form))))
      (op-assign `(op-assign ,(cadr form) ,(caddr form) ,(yl-try-type-tag (cadddr form))))
      (goto      `(goto ,(yl-try-type-tag (cadr form))))
      (if        `(if ,(yl-try-type-tag (cadr form))
                      ,(yl-type-tagger-optimize (caddr form))
                    ;; conditionally splice in the fbranch if it exists
                    ,@(when-let ((fbranch (yl-type-tagger-optimize (cadddr form))))
                        (list fbranch))))
      (otherwise form))))

(defun yl-get-expr-type-tag (expr)
  (and (consp expr)
       (car (member (car expr) '(integer float string)))))

(defun yl-ignore-expr-type-tag (expr)
  (or (and (yl-get-expr-type-tag expr) (cadr expr))
      expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPTIMIZER: CONDITIONALS (IF)
;;
;;  * Transform: `if s then r = b else r = a end`
;;  *     Where: int(a) && int(b) && int(c)
;;  *    Output: `r = (a * 0)^(s + b * s)`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-integer-p (expr)
  (eq (yl-get-expr-type-tag expr) 'integer))

(defun yl-optimize-shortcut-if (condition tbranch fbranch)
  "Optimise an IF `form'. Looks for common usage patterns and
  converts them to a branchless form."
  (if (and (yl-integer-p condition)
           (eq (cadr tbranch) (cadr fbranch))
           (eq 'assign (car tbranch))
           (yl-integer-p (caddr tbranch))
           (eq 'assign (car fbranch))
           (yl-integer-p (caddr tbranch)))
      (let ((s (cadr condition))
            (a (cadr (caddr fbranch)))
            (b (cadr (caddr tbranch))))
        `(assign ,(cadr tbranch) (+ (* ,a (^ 0 ,s)) (* ,b ,s))))
    `(if ,condition ,tbranch ,fbranch)))

(defun yl-optimize-shortcuts (form)
  (let ((sym (car form)))
    (cl-case sym
      (do `(do ,@(mapcar #'yl-optimize-shortcuts (cdr form))))
      (if (yl-optimize-shortcut-if (cadr form) (caddr form) (cadddr form)))
      (otherwise form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPTIMIZER: TYPE TAG STRIPPER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-tag-stripper-optimize (form)
  (let ((sym (car form)))
    (cl-case sym
      (do        `(do ,@(mapcar #'yl-tag-stripper-optimize (cdr form))))
      (assign    `(assign ,(cadr form) ,(yl-ignore-expr-type-tag (caddr form))))
      (op-assign `(op-assign ,(cadr form) ,(caddr form) ,(yl-ignore-expr-type-tag (cadddr form))))
      (goto      `(goto ,(yl-ignore-expr-type-tag (cadr form))))
      (if        `(if ,(yl-ignore-expr-type-tag (cadr form))
                      ,(yl-tag-stripper-optimize (caddr form))
                    ;; conditionally splice in the fbranch if it exists
                    ,@(when-let ((fbranch (yl-tag-stripper-optimize (cadddr form))))
                        (list fbranch))))
      (otherwise form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REARRANGED FORM COMPILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-compile-rearranged (forms)
  (mapcar (lambda (forms)
            (flatten-tree
             (yl-compile-do forms)))
          forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVENIENCE MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-fn (forms)
  (thread-first forms
    ;; Optimization
    (yl-type-tagger-optimize)
    (yl-optimize-shortcuts)

    ;; Optimization cleanup
    (yl-tag-stripper-optimize)

    ;; Expand macros
    (yl-macroexpand)

    ;; Collapse unnecessary nested DOs
    (yl-do-collapser)

    ;; Rearrange toplevel forms to fit the chip
    (toplevel-do-rearranger)

    ;; Compile these rearranged forms
    (yl-compile-rearranged)))

(defmacro yl (&rest forms)
  (let ((result (gensym 'result)))
    `(let ((,result (yl-fn '(do ,@forms))))
       (princ
        (string-join (mapcar #'yl-join-fragments ,result) "\n")))))

(defmacro yl* (&rest forms)
  `(yl-fn '(do ,@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl--test (form expected)
  (cl-assert (equal form expected)))

;; expr compiler tests
(progn
  (yl--test (yl-compile-expr '(== a (+ (+ 10 10) (+ 12 11)))) "a==(10+10+12+11)")
  (yl--test (yl-compile-expr '(== :a :b)) ":a==:b")
  t)

;; primitive statement compiler tests
(progn
  (yl--test (yl* (assign a 1) (assign b 2)) '(("a=1" "b=2")) )
  (yl--test (yl* (if (== a 10) (assign b 4))) '(("if a==10 then b=4 end")))
  (yl--test (yl* (assign :a 10) (// "Comment!")) '((":a=10" "//Comment!")))
  (yl--test (yl* (op-assign z * 2)) '(("z*=2")))
  t)

;; macro expansion tests
(progn
  (yl--test (yl* (set a 1 b 2)) '(("a=1" "b=2")))
  (yl--test (yl* (set z (* z 2))) '(("z*=2")))
  (yl--test (yl* (set :y (+ :y (* z 2)))) '((":y+=z*2")))
  t)

;; rearrangement tests
(progn
  (yl--test (yl* (//-line-length) (//-line-length))'
            (("// <-------------- this line is 70 characters long ------------------>")
             ("// <-------------- this line is 70 characters long ------------------>")))
  (yl--test (yl* (set A 1000 pr 0 div (* (+ 9.6 (* 2.4 pr)) n) so (- 1 sp)
                      o 160000000 e (* 8 o) f 644444444 z (* 12 (* 25 A)) lol 1))
            '(("A=1000" "pr=0" "div=(9.6+2.4*pr)*n" "so=1-sp" "o=160000000" "e=8*o" "f=644444444")
              ("z=12*25*A" "lol=1")))
  (yl--test (yl* (set A 1000 pr 0 div (* (+ 9.6 (* 2.4 pr)) n) so (- 1 sp)
                      o 160000000 e (* 8 o) f 6444444444))
            '(("A=1000" "pr=0" "div=(9.6+2.4*pr)*n" "so=1-sp" "o=160000000" "e=8*o" "f=6444444444")))
  t)

;; optimizer tests
(progn
  (let ((test-form '(if (= a 10) (do (assign b 4) (assign c 3)))))
    (yl--test test-form (yl-tag-stripper-optimize (yl-type-tagger-optimize test-form))))

  (yl--test (yl-type-tagger-optimize '(if 1 (assign b 2) (assign a 3)))
            '(if (integer 1) (assign b (integer 2)) (assign a (integer 3))))

  (yl--test (yl* (if 1 (assign r 2) (assign r 3))) '(("r=3*0^1+2*1")))
  (yl--test (yl* (if 0 (assign r 2) (assign r 3))) '(("r=3*0^0+2*0")))
  t)

;; keep whitespace tests
(progn
  (let ((*yl-separator* " "))
    (yl--test (yl* (if 1 (assign r 2) (assign r 3))) '(("r = 3 * 0 ^ 1 + 2 * 1")))
    (yl--test (yl* (if 0 (assign r 2) (assign r 3))) '(("r = 3 * 0 ^ 0 + 2 * 0"))))
  t)

;; DECLARE and DO tests
(progn
  (yl--test (yl* (do (declare (type integer a)) (set a b))) '(("a=b")))

  ;; DO collapser tests
  (yl--test (yl-do-collapser
             '(do
               (do
                (declare)
                (do
                 (set g h)
                 (do
                  (declare)
                  (set i j))))))
            '(do
              (do
               (declare)
               (set g h)
               (do
                (declare)
                (set i j)))))

  (yl--test (yl-do-collapser
             '(do
               (do
                (do
                 (declare)
                 (set g h)
                 (do
                  (set i j))))))
            '(do
              (do
               (declare)
               (set g h)
               (set i j)))))

(provide 'yololisp-compiler)
;;; yololisp-compiler.el ends here
