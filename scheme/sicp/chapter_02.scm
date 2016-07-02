;;;; Various exercises from chapter 2
;;;;

;;; Church Numerals
(define (zero f)
    (lambda (x) x))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
 
;;;; Exercise 2.6
(define (one f)
  (lambda (f x)))

(define (two f)
  (lambda (x)
    (f (f x))))




  
