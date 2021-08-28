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
            (if (> level 1)
                ;; only apply parentheses to subexpressions of the subexpressions of the first binary op
                (concat "(" result ")")
              result)))
    (symbol  (symbol-name expr))
    (string  expr)
    (integer (number-to-string expr))
    (float   (number-to-string expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YOLOL PRIMITIVE COMPILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-compile-assign (var expr)
  (concat (symbol-name var) " = " (yl-compile-expr expr)))

(defun yl-compile-if (condition tbranch &optional fbranch)
  (if fbranch
      (concat "if "
              (yl-compile-expr condition)
              " then "
              (yl-compile-form tbranch)
              " else "
              (yl-compile-form fbranch)
              " end")
    (concat "if "
            (yl-compile-expr condition)
            " then "
            (yl-compile-form tbranch)
            " end")))

(defun yl-compile-goto (expr)
  (concat "goto " (yl-compile-expr expr)))

(defun yl-compile-comment (comment)
  (concat "// " comment))

(defun yl-compile-do (forms)
  (mapconcat #'identity (mapcar #'yl-compile-form forms) "\n"))

(defun yl-compile-form (form &optional env)
  (let ((sym (car form)))
    (if (yl-get-macro sym)
        (yl-compile-form (yl-macroexpand form))
      (cl-ecase sym
        (do     (yl-compile-do (cdr form)))
        (assign (yl-compile-assign  (cadr form) (caddr form)))
        (if   (yl-compile-if      (cadr form) (caddr form) (cadddr form)))
        (goto (yl-compile-goto    (cadr form)))
        (//   (yl-compile-comment (cadr form)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YOLOLISP-MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl-macro-when (form)
  `(if ,(cadr form) ,(caddr form)))

(defun yl-macro-unless (form)
  `(if (! ,(cadr form)) ,(caddr form)))

(defun yl-macro-set (form)
  (let* ((pairs (-partition-all 2 (cdr form)))
         (assign-pairs (mapcar (lambda (pair) (cons 'assign pair)) pairs)))
    `(do ,@assign-pairs)))

(defvar yl-macro-registry
  '((when   . yl-macro-when)
    (unless . yl-macro-unless)
    (set    . yl-macro-set)))

(defun yl-get-macro (symbol)
  (assoc symbol yl-macro-registry))

(defun yl-macroexpand (form)
  (funcall (cdr (yl-get-macro (car form))) form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVENIENCE MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro yl (&rest forms)
  `(yl-compile-do ',forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yl--test (form expected)
  (cl-assert (string-equal form expected)))

(progn
  ;; expr compiler tests
  (yl--test (yl-compile-expr '(== a (+ (+ 10 10) (+ 12 11)))) "a == (10 + 10) + (12 + 11)")
  (yl--test (yl-compile-expr '(== :a :b)) ":a == :b")
  t)

;; primitive statement compiler tests
(progn
  (yl--test (yl-compile-form '(do (assign a 1) (assign b 2))) "a = 1
b = 2")
  (yl--test (yl-compile-form '(do (if (= a 10) (assign b 4)))) "if a = 10 then b = 4 end")
  (yl--test (yl-compile-form '(do (assign :a 10) (// "Comment!"))) ":a = 10\n// Comment!")
  t)

;; macro expansion tests
(progn
  (yl--test (yl-compile-form '(set a 1 b 2)) "a = 1
b = 2")
  t)
