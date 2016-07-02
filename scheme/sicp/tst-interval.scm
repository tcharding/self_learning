;;;; Test suite for interval.scm
(load "test-framework.scm")
(load "interval.scm")

; initial test of constructor
(define i (make-interval 1 2))

#!
; should cause assertion-violation
(define j (make-interval 3 1))
!#

(define (test-cons-sel)
  (define (test-basic x y)
    "test constructor and selectors"
    (let ((tobj (make-interval x y)))
      (test-eq "lower-bound" (lower-bound tobj) x)
      (test-eq "upper-bound" (upper-bound tobj) y)))

  (let ((raw-data '((1 2) (3 6) (10 1000))))
    (for-each (lambda (ls)
                (test-basic (first ls) (second ls)))
              raw-data)))

(define (test-check-no-span-zero)
  (begin
    (unless (span-zero? (make-interval -1 1))
      (fail "span-zero? failed" #f #t))
    (when (span-zero? (make-interval 1 1))
      (fail "span-zero? failed" #t #f))
    (check-no-span-zero (make-interval 1 1)))) 

(define (test-mul-optimised-interval)
  (define (test-mul x y)
    (let ((mi (mul-interval x y)) (mo (mul-optimised-interval x y)))
      (unless (eq-interval mi mo)
        (fail "mul-optimised not equal to mul-interval" mi mo))))
  (let ((raw-data '((-2 2) (2 3) (-3 -2))))
    (for-each (lambda (ls)
                (display ls)
                (newline)
                (display (car raw-data))
                (newline)
                (test-mul (interval-from-list ls)
                          (interval-from-list (car raw-data)))
 ;               (test-mul ls (cadr raw-data))
  ;              (test-mul ls (caddr raw-data)))
                (newline))
              raw-data)))
              


;;;;
;;;; Run tests
;;;;
(test-cons-sel)
(test-check-no-span-zero)
(test-mul-optimised-interval)
