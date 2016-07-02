;;;; Rational Number Data Abstraction
;;;;
;;;; SICP Section 2.1.1

(load "lib.scm")

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (define (make-rat-pos n d)
    (let ((g (gcd n d)))
      (cons (/ n  g) (/ d g))))
  (let ((rat (make-rat-pos (abs n) (abs d))))
    (cond ((and (neg? n) (neg? d)) rat)
	  ((or (neg? n) (neg? d))
	   (set-car! rat (- 0 (car rat)))
	   rat)
	  (else rat))))

(define (neg? x)
  (if (< x 0) #t #f))

(define (negate-rat x)
  (make-rat (- 0 (numer x)) (denom x)))             

(define (neg-rat? x)
  (cond ((and (neg? (numer x)) (neg? (denom x))) #f)
        ((or (neg? (numer x)) (neg? (denom x))) #t)
        (else #f)))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))
           
