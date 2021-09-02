;; Some sort of YOLOL Lisp-compiler...

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
  "one-arg operators on left-side of value")

(defvar yl-unary-right
  '((inc . "++")
    (dec . "--")
    (fact . "!"))
  "one-arg operators on right-side of value")

(cl-defun yl-compile-expr (expr &optional (level 0))
  "Compiles a YOLOLISP expression `EXPR'.
Optional `LEVEL' refers to nesting level, determining parenthesis use in nested expressions."
  (cl-etypecase expr
    (cons (let ((result (let* ((op (car expr))
                               (arg1 (cadr expr))
                               (arg2 (caddr expr))
                               (left-one-arg-op (cdr (assoc op yl-unary-left)))
                               (right-one-arg-op (cdr (assoc op yl-unary-right))))
                          (cond
                           ;; one argument operation, value on left
                           (left-one-arg-op
                            (concat left-one-arg-op (yl-compile-expr arg1 (1+ level))))
                           ;; one argument operation, value on right
                           (right-one-arg-op
                            (concat (yl-compile-expr arg1 (1+ level)) right-one-arg-op))
                           ;; two-arg binary operation
                           (t
                            (concat (yl-compile-expr arg1 (1+ level))
                                    " "
                                    (symbol-name op)
                                    " "
                                    (yl-compile-expr arg2 (1+ level)))))
                          )))
            (if (> level 0)
                ;; only apply parentheses to subexpressions of the subexpressions of the first binary op
                (concat "(" result ")")
              result)))
    (symbol  (symbol-name expr))
    (string  (s-replace-all '(("\n" . "\\n")) (concat "\"" expr "\"")))
    (integer (number-to-string expr))
    (float   (number-to-string expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YOLOL PRIMITIVE COMPILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some nomenclature: a "chunk" of YOLOL is a string of output YOLOL like "a =
;; 10" or "if a = 1 then b = 1 end". The compiler outputs lists of these YOLOL
;; chunks, which can then be rearranged to meet chip code size limits.

(defun yl-join-chunks (forms &optional separator)
  "Joins the compilation result (`FORMS') of YOLOLISP's DO form.

Accepts an optional `SEPARATOR' string."
  (s-join (or separator " ") (-flatten forms)))

(defun yl-compile-if (condition tbranch &optional fbranch)
  ""
  ;; Branches are compiled with YL-COMPILE-FORM, and the resulting chunks are
  ;; joined with YL-JOIN-CHUNKS. We join because YOLOL's IF constrains the
  ;; branch code to a single line, so we have to treat IF as a single "chunk",
  ;; even though the branch code consists of viable chunks.
  ;; (...in future we ought to retain the chunks for analysis reasons)
  (if fbranch
      (concat "if "
              (yl-compile-expr condition)
              " then "
              (yl-join-chunks (yl-compile-form tbranch))
              " else "
              (yl-join-chunks (yl-compile-form fbranch))
              " end")
    (concat "if "
            (yl-compile-expr condition)
            " then "
            (yl-join-chunks (yl-compile-form tbranch))
            " end")))

(defun yl-compile-assign (var expr)
  (concat (symbol-name var) " = " (yl-compile-expr expr)))

(defun yl-compile-op-assign (var op expr)
  (cl-ecase op
    ((+ - * /)
     (concat (symbol-name var) (format " %s= " op) (yl-compile-expr expr)))))

(defun yl-compile-goto (expr)
  (concat "goto " (yl-compile-expr expr)))

(defun yl-compile-comment (comment)
  (concat "//" comment))

(defun yl-compile-do (forms)
  (mapcar #'yl-compile-form forms))

(defun yl-compile-form (form &optional env)
  (let ((sym (car form)))
    (if (yl-get-macro sym)
        (yl-compile-form (yl-macroexpand form))
      (cl-ecase sym
        (do     (yl-compile-do (cdr form)))
        (if     (list (yl-compile-if (cadr form) (caddr form) (cadddr form))))
        (assign (list (yl-compile-assign  (cadr form) (caddr form))))
        (op-assign (list (yl-compile-op-assign (cadr form) (caddr form) (cadddr form))))
        (goto   (list (yl-compile-goto    (cadr form))))
        (//     (list (yl-compile-comment (cadr form))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YOLOLISP-MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-macro-when (form)
  `(if ,(cadr form) ,(caddr form)))

(defun yl-macro-unless (form)
  `(if (! ,(cadr form)) ,(caddr form)))

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
  (let* ((pairs (-partition-all 2 (cdr form)))
         (assign-pairs (mapcar #'yl-transform-assign-pair pairs)))
    `(do ,@assign-pairs)))

(defun yl-macro-comment-line-length (form)
  `(// " <-------------- this line is 70 characters long ------------------>"))

(defvar yl-macro-registry
  '((when   . yl-macro-when)
    (unless . yl-macro-unless)
    (set    . yl-macro-set)
    (//-line-length . yl-macro-comment-line-length)))

(defun yl-get-macro (symbol)
  (assoc symbol yl-macro-registry))

(defun yl-macroexpand (form)
  (funcall (cdr (yl-get-macro (car form))) form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE SIZE CONSTRAINER / CHUNK REARRANGER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chunk-rearranger (chunks &optional column-width)
  "Rearranges compiler output `CHUNKS' into `COLUMN-WIDTH' columns (default 70).
Returns a list of lists of chunks constrained to columns, ready
for concatenation into an output YOLOLISP file."
  (cl-loop
   ;; CONSTRAINED-CHUNK-LISTS is a list of chunk lists that are
   ;; column-constrained. Each entry corresponds to a line of output YOLOL.
   with constrained-chunk-lists = (list nil)

   ;; Loop over all of our chunks. Chunk nesting order is not important to us
   ;; here, so we flatten away the hierarchy.
   for current-chunk in (-flatten chunks)

   for current-line-chunks = (car constrained-chunk-lists)
   ;; sum current line's chunks and factor in spacing to get total line's length
   ;; (the final chunk has no space appended, hence the 1-)
   for total-line-length = (+ (-sum (mapcar #'length current-line-chunks))
                              (1- (length current-line-chunks)))

   ;; when we exceed the current line's column limit, create a new line, and
   ;; repoint to it
   when (>= (+ total-line-length (length current-chunk)) (or column-width 70))
   do
     (push nil constrained-chunk-lists)

   ;; and finally, add the chunk to the appropriate line
   do
     (push current-chunk (car constrained-chunk-lists))

   ;; correct the ordering of all of these pushes by reversing everything
   finally return (nreverse (mapcar #'nreverse constrained-chunk-lists))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPTIMIZER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-optimize (form)
  ;; TODO: framework here
  form)

(defun yl-optimize-if (form)
  "Optimise an IF `form'. Looks for common usage patterns and
  converts them to a branchless form."
  (let ((condition (cadr   form))
        (tbranch   (caddr  form))
        (fbranch   (cadddr form)))
    (if (and (yl-integer-p condition)
             (eq (cadr tbranch) (cadr fbranch))
             (eq 'assign (car tbranch))
             (yl-integer-p (caddr tbranch))
             (eq 'assign (car fbranch))
             (yl-integer-p (caddr tbranch)))
        (let ((s (cadr  form))
              (a (caddr fbranch))
              (b (caddr tbranch)))
          `(assign ,(cadr tbranch) (^ (* ,a 0) (* (+ ,s ,b) ,s))))
      form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVENIENCE MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-fn (forms)
  (let ((chunk-lists (chunk-rearranger (yl-compile-form forms))))
    (s-join "\n" (mapcar (-partial #'s-join " ") chunk-lists))))

(defmacro yl (&rest forms)
  `(progn (princ (yl-fn '(do ,@forms))) nil))

(defmacro yl* (&rest forms)
  `(chunk-rearranger (yl-compile-form '(do ,@forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl--test (form expected)
  (cl-assert (equal form expected)))

;; expr compiler tests
(progn
  (yl--test (yl-compile-expr '(== a (+ (+ 10 10) (+ 12 11)))) "a == ((10 + 10) + (12 + 11))")
  (yl--test (yl-compile-expr '(== :a :b)) ":a == :b")
  t)

;; primitive statement compiler tests
(progn
  (yl--test (yl* (assign a 1) (assign b 2)) '(("a = 1" "b = 2")))
  (yl--test (yl* (if (= a 10) (assign b 4))) '(("if a = 10 then b = 4 end")))
  (yl--test (yl* (assign :a 10) (// "Comment!")) '((":a = 10" "//Comment!")))
  (yl--test (yl* (op-assign z * 2)) '(("z *= 2")))
  t)

;; macro expansion tests
(progn
  (yl--test (yl* (set a 1 b 2)) '(("a = 1" "b = 2")))
  (yl--test (yl* (set z (* z 2))) '(("z *= 2")))
  (yl--test (yl* (set :y (+ :y (* z 2)))) '((":y += z * 2")))
  t)

;; rearrangement tests
(progn
  (yl--test (yl* (//-line-length) (//-line-length))'
            (("// <-------------- this line is 70 characters long ------------------>")
             ("// <-------------- this line is 70 characters long ------------------>")))
  (yl--test (yl* (set A 1000 pr 0 div (* (+ 9.6 (* 2.4 pr)) n) so (- 1 sp)
                      o 160000000 e (* 8 o)))
            '(("A = 1000" "pr = 0" "div = (9.6 + (2.4 * pr)) * n" "so = 1 - sp"
               "o = 160000000")
              ("e = 8 * o")))
  (yl--test (yl* (set A 1000 pr 0 div (* (+ 9.6 (* 2.4 pr)) n) so (- 1 sp)
                      o 1600000000 e (* 8 o)))
            '(("A = 1000" "pr = 0" "div = (9.6 + (2.4 * pr)) * n" "so = 1 - sp")
              ("o = 1600000000" "e = 8 * o")))
  t)

;; optimizer tests
(progn
  ;; ;; ENABLE WHEN WE HAVE DECLARE AND IF-OPTIMIZATION
  ;; (yl--test (yl*
  ;;            (do
  ;;             (declare (type integer cond res a b))
  ;;             (if cond (assign res b) (assign res a))))
  ;;           '("res = (a * 0) ^ ((cond + b) * cond)"))
  t)
