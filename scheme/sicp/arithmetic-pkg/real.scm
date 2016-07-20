;;;; Real Numbers

(define (real? x)
  (eq? (type-tag x) 'real))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  
  (put 'add
       '(real real)
       (lambda (x y)
         (tag (+ x y))))
  
  (put 'sub
       '(real real)
       (lambda (x y)
         (tag (- x y))))
  
  (put 'mul
       '(real real)
       (lambda (x y)
         (tag (* x y))))
  
  (put 'div
       '(real real)
       (lambda (x y)
         (tag (/ x y))))
  
  (put 'num-eq?
       '(real real)
       (lambda (x y)
         (equal-float? x y)))
  
  (put '=zero?
       '(real)
       (lambda (x)
         (= x 0)))
  
  (put 'maximum
       '(real real)
       (lambda (x y)
         (tag (max x y))))

  (put 'minimum
       '(real real)
       (lambda (x y)
         (tag (min x y))))
  
  (put 'lt
       '(real real)
       (lambda (x y)
         (< x y)))
  
  (put 'gt
       '(real real)
       (lambda (x y)
         (> x y)))

  (put 'exponential
       '(real real)
       (lambda (b e)
         (expt b e)))

  (put 'make
       'real
       (lambda (x)
         (tag (* x 1.0))))
  'done)
