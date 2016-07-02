;;;; Alternate implementation of a pair of non-negative integers
;;;;
;;;; Exercise 2.5

;; Pair (A B) is represented by 2eA.3eB

(define (cons-pair x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car-pair p)
  (car (a.b p)))

(define (cdr-pair p)
  (cdr (a.b p)))

(define (a.b p)
  "returns (a b)"
  (let ((half-baked (a.e3b p)))
    (cons (car half-baked) (threes-into (cadr half-baked)))))

(define (a.e3b p)
  "returns (a, 2eB)"
  (let fn ((a 0) (b3 p))
    (cond ((odd? b3) (list a b3))
          (else (fn (inc a) (/ b3 2))))))

(define (threes-into n)
  "n = 3eX, returns X"
  (let fn ((x 0) (n n))
    (cond ((= n 1) x)
          (else (fn (inc x) (/ n 3))))))

     
        
