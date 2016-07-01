;;;; Procedures for working with prime numbers

(define (runtime) (tms:clock (times)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (remainder a b)
  "return remainder of a / b"
  (- a (* (quotient a b) b)))

;;;; Fermat's Little Theorem

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
	 (let ((tmp (square (expmod base (/ exp 2) m))))
	   (if (non-trivial-square-root tmp exp)
	       0
	       (remainder tmp m))))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; Exercise 1.28, page 56
(define (non-trivial-square-root v n)
  "No idea what is meant by 'non-trivial square root'?"
  #f)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  "run Fermat's test n times"
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;;;; Exercise 1.22

(define (timed-prime-test n)
  (let ((start-time (runtime)))
    (start-prime-test n start-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (begin
	(display n)
	(report-prime (- (runtime) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes start stop)
  (let fn ((n start))
    (cond ((< n stop)
	   (timed-prime-test n)
	   (fn (next-odd n))))))

(define (next-odd n)
  (if (odd? n)
      (+ n 2))
      (+ n 1))

(define (find-n-primes-bigger-than n start)
  "find 3 primes bigger than start"
  (let fn ((n n) (try start))
    (if (> n 0)
	(cond ((fast-prime? try 5)
	       (timed-prime-test try)
	       (fn (- n 1) (next-odd try)))
	      (else (fn n (next-odd try)))))))


(define (next n)
  (if (= n 2)
      3
      (+ n 2)))
	     
   
	    
