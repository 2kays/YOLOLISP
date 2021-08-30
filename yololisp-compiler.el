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
    (string  (concat "\"" expr "\""))
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
        (goto   (list (yl-compile-goto    (cadr form))))
        (//     (list (yl-compile-comment (cadr form))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YOLOLISP-MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-macro-when (form)
  `(if ,(cadr form) ,(caddr form)))

(defun yl-macro-unless (form)
  `(if (! ,(cadr form)) ,(caddr form)))

(defun yl-macro-set (form)
  (let* ((pairs (-partition-all 2 (cdr form)))
         (assign-pairs (mapcar (lambda (pair) (cons 'assign pair)) pairs)))
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

(defun chunk-rearranger (chunks)
  "Rearranges compiler output `CHUNKS' into 70-column width."
  ;; FIXME: indecipherable LOOP black magic
  ;; FIXME: every line has a space at the end (the `amended' var is a hack!!)
  (cl-loop with flattened = (-flatten chunks)
           with amended = (append (mapcar (lambda (s) (concat s " ")) (butlast flattened))
                                  (last flattened))
           with accum = 0
           with lizt = (list nil)
           for elem in amended
           do
           (if (> (+ accum (length elem)) 70)
               (progn
                 (print (concat "Stopped at " (number-to-string accum)))
                 (setq accum (length elem))
                 (setf (car lizt) (nreverse (car lizt)))
                 (push nil lizt)
                 (push elem (car lizt)))
             (cl-incf accum (length elem))
             (push elem (car lizt)))
           finally return (nreverse lizt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVENIENCE MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-fn (forms)
  (cl-flet ((join (l) (mapconcat #'identity l ""))
            (join-line  (l) (mapconcat #'identity l "\n")))
    (join-line
     (mapcar #'join (chunk-rearranger (yl-compile-form forms))))))

(defmacro yl (&rest forms)
  `(yl-fn ',forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl--test (form expected)
  (cl-assert (equal form expected)))

(progn
  ;; expr compiler tests
  (yl--test (yl-compile-expr '(== a (+ (+ 10 10) (+ 12 11)))) "a == ((10 + 10) + (12 + 11))")
  (yl--test (yl-compile-expr '(== :a :b)) ":a == :b")
  t)

;; primitive statement compiler tests
(progn
  (yl--test (yl-compile-form '(do (assign a 1) (assign b 2))) '(("a = 1") ("b = 2")))
  (yl--test (yl-compile-form '(do (if (= a 10) (assign b 4)))) '(("if a = 10 then b = 4 end")))
  (yl--test (yl-compile-form '(do (assign :a 10) (// "Comment!"))) '((":a = 10") ("//Comment!")))
  t)

;; macro expansion tests
(progn
  (yl--test (yl-compile-form '(set a 1 b 2)) '(("a = 1") ("b = 2")))
  t)
