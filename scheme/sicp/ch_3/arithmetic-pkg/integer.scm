(define (integer? x)
  (eq? (type-tag x) 'integer))

(define (install-integer-package)

  (define (gcd-integer x y)
  "Euclid's Algorithm for finding GCD"
  (let ((x (abs x))
        (y (abs y)))
    (if (< x y)
        (gcd-integer y x)
        (if (= y 0)
            x
            (gcd-integer y (remainder x y))))))
  
  (define (reduce-integer x y)
    (let ((g (gcd-integer x y)))
      (list (div x g)
            (div y g))))
  
  (define (tag x)
    (attach-tag 'integer x))
  
  (put 'add
       '(integer integer)
       (lambda (x y)
         (tag (round (+ x y)))))
  
  (put 'sub
       '(integer integer)
       (lambda (x y)
         (tag (round (- x y)))))
  
  (put 'mul
       '(integer integer)
       (lambda (x y)
         (tag (round (* x y)))))
  
  (put 'div
       '(integer integer)
       (lambda (x y)
         (tag (round (/ x y)))))
  
  (put 'num-eq?
       '(integer integer)
       (lambda (x y)
         (= x y)))
  
  (put '=zero?
       '(integer)
       (lambda (x)
         (= x 0)))
  
  (put 'lt
       '(integer integer)
       (lambda (x y)
         (< x y)))
  
  (put 'gt
       '(integer integer)
       (lambda (x y)
         (> x y)))

  (put 'exponential
       '(integer integer)
       (lambda (b e)
         (expt b e)))

  (put 'reduce
       '(integer integer)
       (lambda (x y)
         (let ((reduced (reduce-integer x y)))
           (list (car reduced) (cadr reduced)))))

  (put 'greatest-common-divisor
       '(integer integer)
       (lambda (x y)
         (tag (gcd-integer x y))))

  (put 'maximum
       '(integer integer)
       (lambda (x y)
         (tag (max x y))))

  (put 'minimum
       '(integer integer)
       (lambda (x y)
         (tag (min x y))))

    (put 'absolute
       '(integer)
       (lambda (x)
         (tag (abs x))))

    (put 'remainder
         '(integer integer)
         (lambda (x y)
           (tag (remainder x y))))

    (put 'quotient
         '(integer integer)
         (lambda (x y)
           (tag (quotient x y))))

  (put 'make 'integer
       (lambda (x) (tag (round x))))
  'done)
