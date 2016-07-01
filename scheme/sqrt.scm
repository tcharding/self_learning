(define sqrt-iter
  (lambda (guess x previous-guess)
    (if (good-enough? guess x previous-guess)
        guess
        (sqrt-iter (improve guess x) x guess))))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x 10.0))

(define good-enough?
  (lambda (guess x previous-guess)
    (< (abs (- guess previous-guess)) 0.001)))

(define square
  (lambda (x)
    (* x x)))


;; doesn't work because then-clause is evaluated before predicate
(define new-if
  (lambda (predicate then-clause else-clause)
    (cond
     (predicate then-clause)
     (else else-clause))))
