;;;; Various exercises from chapter 1
;;;;

(import (rnrs lists))
(load "test-suite.scm")

(define (runtime)
  (tms:clock (times)))

(define (inc i)
  (+ i 1))

(define (dec i)
  (- i 1))

;;;; Exercise 1.3

(define sum-of-squares-of-largest-two
  (lambda (x y z)
    (sum-of-squares (largest-two (list x y z)))))

(define sum-of-squares
  (lambda (ls)
    (cond
     ((null? ls) 0)
     (else (+ (square (car ls)) (sum-of-squares (cdr ls)))))))

(define largest-two
  (lambda (ls)
    (let ((m (maximum ls)))
          (list m (maximum (remove m ls))))))

(define square
  (lambda (x)
    (* x x)))

(define maximum
  (lambda (ls)
    (let fn ((max-so-far (car ls)) (ls ls))
      (cond
       ((null? ls) max-so-far)
       (else (if (> (car ls) max-so-far)
		 (fn (car ls) (cdr ls))
		 (fn max-so-far (cdr ls))))))))

;;;; Exercise 1.9

(define (plus-fn1 a b)
  (if (= a 0)
      b
      (inc (plus-fn1 (dec a) b))))

(define inc
  (lambda (n)
    (+ n 1)))

(define (dec n)
  (- n 1))

(define plus-fn2
  (lambda (a b)
    (if (= a 0)
        b
        (plus-fn2 (dec a) (inc b))))) 

;;;; Exercise 1.11

(define (fn-r n)
  "recursive version"
  (cond
   ((< n 3) n)
   (else (+ (fn-r (- n 1))
            (fn-r (- n 2))
            (fn-r (- n 3))))))

(define (fn n)
  "iterative version"
  (let f ((n n) (a 0) (b 1) (c 2))
    (cond
     ((zero? n) a)
     (else (f
            (- n 1)
            b
            c
            (+ a b c))))))

;;;; Exercise 1.17

(define (mul-r a b)
  "Recursive version"
  (if (= b 0)
      0
      (+ a (mul-r a (- b 1)))))

(define (mul-lg a b)
  "Big O log n version"
  (cond
   ((= b 0) 0)
   ((even? b) (double (mul-lg a (halve b))))
   (else (+ a (mul-lg a (- b 1))))))

(define (halve x)
  (if (even? x)
      (/ x 2)
      (error 'halve "error, argument must be even")))

(define (double x)
  (+ x x))

;;;; Exercise 1.18 (incomplete)

(define (mul-it x y)
  "iterative Big O n version"
  (let fn ((x x) (y y) (ac 0))
    (cond
     ((= y 0)  ac)
;     ((even? y) (double (fn x (halve y) ac)))
     (else (fn x (- y 1) (+ ac x))))))

(define (mul-lg x y)
  "recursive Big O log n version"
  (cond
   ((= y 0) 0)
   ((even? y) (double (mul-lg x (halve y))))
   (else (+ x (mul-lg x (- y 1))))))

(define (debug . x)
  (define display-obj
    (lambda obj
      (display obj)
      (newline)))
    (for-each display-obj x)
    (newline))

(define (test-mul-it? tc exp)
  "returns (tc exp got) on error"
  (define (next-result tc)
    (mul-it (caar tc) (cadar tc)))
  (let fn ((tc tc) (exp exp))
    (cond
     ((null? tc) #t)
     ((not (= (car exp) (next-result tc)))
	(list (car tc) (car exp) (next-result tc)))
     (else (fn (cdr tc) (cdr exp))))))

(define (test-mul-it)
  (let ((tc '((2 2) (2 3) (3 2) (3 4) (4 3) (5 8) (7 8) (2 10) (10 3) (10 10) (100 100)))
	(exp '(4 6 6 12 12 40 56 20 30 100 10000)))
    (test-mul-it? tc exp)))

;;;; Exercise 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q))
		   (+ (* 2 (* p q)) (square q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))


(define (gcd a b)
  "Euclid's Algorithm for finding GCD"
  (if (< a b)
      (gcd b a)
      (if (= b 0)
	  a
	  (gcd b (remainder a b)))))

(define (remainder a b)
  "return remainder of a / b"
  (if (< a b)
      (remainder b a)
      (- a (* (quotient a b) b))))

;;;; Exercise 1.29

;; (define (sum term a next b)
;;   (if (> a b)
;;       0
;;       (+ (term a)
;; 	 (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (inc n)
  (+ n 1))

(define (cube x)
  (* x x x))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;;;; Exercise 1.29

;; Calculate integral between 0 and 1 using Simpsons rule
;; (simpsons-rule cube 0 1 1000)

(define (simpsons-rule f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (let ((y (f (+ a (* k h)))))
        (cond
         ((= k 0) y)
         ((odd? k) (* 4 y))
         (else (* 2 y)))))
    (* (/ h 3.0)
       (sum term 0 inc n))))

;;;; Exercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;;;; Exercise 1.31

(define (product-r term a next b)
  (if (> a b)
      1
      (* (term a)
	 (sum term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-on-4 n)
  (define (term k)
    (cond ((= k 0) 2/3)
          (else (/ (num-term k) (denom-term k)))))
  (define (num-term k)
    (if (odd? k)
        (+ 3 k)
        (+ 2 k)))
  (define (denom-term k)
    (if (odd? k)
        (- (num-term k) 1)
        (+ (num-term k) 1)))
  (product term 0 inc n))

;;;; Exercise 1.31

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-r combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;;; Exercise 1.32

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (let* ((t (term a)) (pass (filter a)) (nr (next a)))
          (if pass
              (iter nr (combiner (term a) result))
              (iter nr result)))))
  (iter a null-value))

(load "primes.scm")
(define (sum-squares-primes a b)
  "Exercise 1.32 part a"
  (filtered-accumulate + 0 square a inc b prime?))

(define (sum-numbers a b)
  (filtered-accumulate + 0 identity a inc b number?))

(define (product-relative-primes a b)
    "Exercise 1.32 part b"
    (define (relatively-prime? n)
      (= 1 (gcd n b)))
    (filtered-accumulate * 1 identity a inc b relatively-prime?))

;;;; Exercise 1.35 and 1.36

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
;      (display guess)
;      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (goldern-ration)
  (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1))

(define log-first-guess 0.9)            ; we can't us 1 since log 1 = 0
(define (x-to-the-x=1000)
  (fixed-point (lambda (x) (average x (abs (/ (log 1000) (log x))))) log-first-guess))

(define (average x y)
  (/ (+ x y) 2))

;;;; Exercise 1.37


;;
;; Cannot figure out how to evaluate the returned expression
;;
(define (cont-frac n d k)
  "calculate k-term finite continued fraction"
    (force (build-kterm-exp n d k)))


(define (build-kterm-exp n d k)
  (delay
    (let fn ((ls '()) (i k))
      (cond ((= i 0) ls)
            (else (fn (add-ls-to-exp ls (build-exp n d i))
                      (dec i)))))))
  
; e is of form (/ N (+ D))
(define (add-ls-to-exp ls e)
  (cond ((null? ls)
         (cons (/-e e)
               (cons (n-e e)
                     (cons (d-exp e) ls))))
        (else  (cons (/-e e)
                     (cons (n-e e)
                           (list (cons (+-e e)
                                       (cons (d-e e)
                                             (list ls)))))))))

(define (/-e e) (car e))
(define (n-e e) (cadr e))
(define (+-e e) (car (d-exp e)))
(define (d-e e) (cadr (d-exp e)))
(define (d-exp e) (caddr e))


(define (test-add-ls-to-exp)
  (let ((ls '()) (e '(/ n (+ d))))
    (begin
      (tst-eq "add single expression to empty list"
              '(/ n (+ d))
              (add-ls-to-exp ls e))
      (tst-eq "add single expression to list of one expression"
              '(/ n (+ d (/ N (+ D))))
              (add-ls-to-exp '(/ N (+ D)) e)))))
      
(define (build-exp n d i)
  (cons '/ (cons (n i) (list (list '+ (d i))))))

;;;; Exercise 1.38
(import (rnrs r5rs))
(define (eulers-expansion k)
  (define (n i) 1)
  (define (d i)
    (cond ((= i 1) 1)
          ((= i 2) 2)
          (else
           (let ((i (- i 2)))
             (cond
              ((= 0 (modulo i 3))
               (+ (* (/ i 3) 2) 2))
              (else 1))))))
  
  (cont-frac n d k))

;;;; Exercise 1.39

(define (tan x k)
  "approximate tan x using k-term finite continued fraction"
  (define (n i)
    (expt x i))
  (define (d i)
    (dec (double i)))
  (cont-frac n d k))

;;;; Section 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
            
;;;; Exercise 1.40

; (newtons-method (cubic a b c) 1)

;; simple cubic polynomial
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;;;; Exercise 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

;;;; Exercise 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;;; Exercise 1.43

(define (repeated f n)
  (lambda (x)
    (let fn ((f f) (n n))
      (cond ((= n 1) (f x))
            (else (fn (compose f f) (dec n)))))))

;;;; Exercise 1.44

;; dx defined above to 0.00001

(define (n-fold f n)
  (repeated (smooth f) n))

(define (smooth f)
    (lambda (x) (average (f (- x dx))
                         (f x)
                         (f (+ x dx)))))

(define average
  (lambda x
    (/ (sum-ls x) (length x))))

(define sum-ls
  (lambda (ls)
    (cond ((null? ls) 0)
          (else (+ (car ls) (sum-ls (cdr ls)))))))
   
;;;; Exercise 1.45

(define (cube-roots x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (fourth-roots x)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (cube y)))) 2)
               1.0))

(define (nth-roots x n n-damp)
  (fixed-point (repeated (average-damp (root-fn x (dec n))) n-damp)
               1.0))

(load "exp.scm")
(define (root-fn x power)
  (lambda (y)
    (/ x (fast-expt y power))))

;;;; Exercise 1.46

(define (iterative-improve good-enough improve x)
  (lambda (guess)
    (let fn ((guess guess))
      (let ((next-guess (improve guess x)))
        (if (good-enough next-guess guess)
            guess
            (fn next-guess))))))

(define (sqrt x)
  ((iterative-improve good-enough? improve x) 10.0))              

(define good-enough?
  (lambda (guess previous-guess)
    (< (abs (- guess previous-guess)) 0.001)))

(define (improve guess x)
  (average guess (/ x guess)))


