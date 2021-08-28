;; Some sort of YOLOL Lisp-compiler...

(defvar yl-unary-left
  '((neg . "-")
    (sqrt . "sqrt ")
    (abs . "abs ")
    (not . "not ")
    (! . "!")
    (inc . "++")
    (dec . "--")
    ;; trig
    (sin . "sin ")
    (cos . "cos ")
    (tan . "tan ")
    (asin . "asin ")
    (acos . "acos ")
    (atan . "atan "))
  "one-arg operators on left-side of value")

(defvar yl-unary-right
  '((rinc . "++")
    (rdec . "--")
    (fact . "!"))
  "one-arg operators on right-side of value")

(cl-defun yl-compile-expr (expr &optional (level 0))
  "Compiles a YOLOLISP expression `expr'."
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

(defun yl-compile-set (var expr)
  (concat (symbol-name var) " = " (yl-compile-expr expr)))

(defun yl-compile-when (condition tbranch)
  (concat "if "
          (yl-compile-expr condition)
          " then "
          (yl-compile tbranch)
          " end"))

(defun yl-compile-if (condition tbranch fbranch)
    (concat "if "
          (yl-compile-expr condition)
          " then "
          (yl-compile tbranch)
          " else "
          (yl-compile fbranch)
          " end"))

(defun yl-compile-goto (expr)
  (concat "goto " (yl-compile-expr expr)))

(defun yl-compile-comment (comment)
  (concat "// " comment))

(defun yl-compile-form (form &optional env)
  (let ((sym (car form)))
    (cl-ecase sym
      (set  (yl-compile-set     (cadr form) (caddr form)))
      (when (yl-compile-when    (cadr form) (caddr form)))
      (if   (yl-compile-if      (cadr form) (caddr form) (cadddr form)))
      (goto (yl-compile-goto    (cadr form)))
      (//   (yl-compile-comment (cadr form))))))

(defun yl-compile-forms (forms)
  (mapconcat #'identity (mapcar #'yl-compile-form forms) "\n"))

(defmacro yl (&rest forms)
  `(yl-compile-forms ',forms))


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

;; statement compiler tests
(progn
  (yl--test (yl-compile-form '(when (= a 10) (set b 4))) "if a = 10 then b = 4 end")
  (yl--test (yl-compile-forms '((set :a 10) (// "Poooop!"))) ":a = 10\n// Poooop!")
  t)
