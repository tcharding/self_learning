;;;; Real Numbers

(define real-tag 'real)
(define (real? x)
  (eq? (type-tag x) real-tag))

(define (install-real-package)
  (define (tag x)
    (attach-tag real-tag x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'num-eq? '(real real)
       (lambda (x y) (equal-float? x y)))
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'make 'real
       (lambda (x) (tag (* x 1.0))))
  'done)
