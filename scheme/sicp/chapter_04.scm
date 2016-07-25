;;;; Various exercises from chapter 4
;;;;

(load-from-path "lib.scm")
(load-from-path "interpreter.scm")x

;;;; Exercise 4.1

(define (list-of-values-lr exps env)
  "left to right evaluation"
  (if (no-operands? exps)
      '()
      (let ((res (eval (first-operand exps) env)))
        (cons res
              (list-of-values-lr (rest-operands exps) env)))))

(define (list-of-values-rl exps env)
  "right to left evaluation"
  (if (no-operands? exps)
      '()
      (let ((res (list-of-values-rl (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              res))))
