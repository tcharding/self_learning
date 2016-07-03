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
        (fail "mul-optimised not equal to mul-interval" mo mi))))
  (let ((raw-data '((-2 2) (2 3) (-3 -2))))
    (for-each (lambda (ls)
                (test-mul (interval-from-list ls)
                          (interval-from-list (car raw-data)))
                (test-mul (interval-from-list ls)
                          (interval-from-list (cadr raw-data)))
                (test-mul (interval-from-list ls)
                          (interval-from-list (caddr raw-data))))
              raw-data)))
              
(define (interval-from-list ls)
  (make-interval (first ls) (second ls)))

(define (test-constructors)
  (define (check-equal x y)
    (when (not (eq-interval x y))
      (av 'check-equal "intervals not equal" x y)))
  (let ((i1 (make-interval 5.4 6.6))
        (i2 (make-center-percent 6 10))
        (i3 (make-center-percent 6 0.10))
        (i4 (make-center-width 6 0.6)))
    (check-equal i1 i2)
    (check-equal i1 i3)
    (check-equal i1 i4)))
    

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (test-par)
  "par1 and par2 should give the same answer"
  (let ((i1 (make-center-percent 6 0.01))
        (i2 (make-center-percent 4 0.02)))
    (unless (eq-interval (par1 i1 i2) (par2 i1 i2))
      (av 'test-par
          "par1 and par2 give differing results"
          (par1 i1 i2)
          (par2 i1 i2)))))

;;;;
;;;; Run tests
;;;;
(test-cons-sel)
(test-check-no-span-zero)
(test-mul-optimised-interval)
(test-constructors)
(test-par)
