;;;; Routines created during completion of SICP
;;;;
;;;; NOTE: code may be taken directly from text

(define (runtime)
  (tms:clock (times)))

(define (inc i)
  (+ i 1))

(define (dec i)
  (- i 1))

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

(define first car)
(define (second ls)
  (cadr ls))

(define d display)
(define (dnl x)
  (display x)
  (newline))



