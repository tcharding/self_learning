(define cube-root-iter
  (lambda (guess x previous-guess)
    (if (good-enough? guess x previous-guess)
        guess
        (cube-root-iter (improve guess x) x guess))))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (+ guess guess))
     3))

(define (average x y)
  (/ (+ x y) 2))

(define (cube-root x)
  (cube-root-iter 1.0 x 10.0))

(define good-enough?
  (lambda (guess x previous-guess)
    (< (abs (- guess previous-guess)) 0.001)))

(define square
  (lambda (x)
    (* x x)))
