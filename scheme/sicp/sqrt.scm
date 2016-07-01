;;;; Find square root using Newtons Method
;;;;
;;;; sicp page 23

(define (sqrt x)
  (sqrt-iter 1.0 x 10.0))

(define sqrt-iter
  (lambda (guess x previous-guess)
    (if (good-enough? guess x previous-guess)
        guess
        (sqrt-iter (improve guess x) x guess))))

(define good-enough?
  (lambda (guess x previous-guess)
    (< (abs (- guess previous-guess)) 0.001)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
