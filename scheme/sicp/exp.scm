;;;; Exponentiation
;;;;
;;;; sicp: 1.2.4 page 44

(define (expt-r b n)
  "b to the power of n, recursive version"
  (if (= n 0)
      1
      (* b (expt-r b (- n 1)))))

(define (expt b n)
  "b to the power of n"
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))
  
(define (fast-expt-r b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt-r b (/ n 2))))
	(else (* b (fast-expt-r b (- n 1))))))

(define (square x)
  (* x x))

;;;; Exercise 1.16

(define (fast-expt b n)
  (let fn ((b b) (n n) (a 1))
    (cond ((= n 0) a)
	((even? n)
	 (fn (square b) (/ n 2) a))
	(else
	 (fn b (- n 1) (* a b))))))

