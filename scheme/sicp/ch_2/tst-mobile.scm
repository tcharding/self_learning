;;;; tests for mobile.scm
(load "../test-framework.scm")
(load "mobile.scm")

;;; test constructors
(define b1 (make-branch 1 2))
(define b2 (make-branch 4 8))
(define b3 (make-branch 16 32))

(define sub (make-mobile b2 b3))
(define root (make-mobile b1 (make-branch 2 sub)))

;;; test Selectors

(test-eq "left-branch" (left-branch sub) b2)
(test-eq "right-branch" (right-branch sub) b3)

(test-eq "branch-payload" (branch-payload (left-branch sub)) 8)
(test-eq "branch-payload" (branch-payload (right-branch sub)) 32)

(test-eq "branch-length" (branch-length b1) 1)
(test-eq "branch-length" (branch-length b2) 4)
(test-eq "branch-length" (branch-length (right-branch root)) 2)

;;; test Equality

(test-eq "branch-eq?" (branch-eq? b1 b1) #t)
(test-eq "branch-eq?" (branch-eq? b1 b2) #f)

(test-eq "mobile-eq?" (mobile-eq? sub sub) #t)
(test-eq "mobile-eq?" (mobile-eq? sub root) #f)
(test-eq "mobile-eq?" (mobile-eq? root root) #t)

;;; test weight related procedures

(test-eq "weight?" (weight? (branch-payload b1)) #t)
(test-eq "weight?" (weight? (branch-payload b2)) #t)

(test-eq "holds-weight?" (holds-weight? (left-branch sub)) #t)
(test-eq "holds-weight?" (holds-weight? (right-branch sub)) #t)

(test-eq "holds-weight?" (holds-weight? (left-branch root)) #t)
(test-eq "holds-weight?" (holds-weight? (right-branch root)) #f)

(test-eq "branch-weight" (branch-weight b1) 2)      
(test-eq "branch-weight" (branch-weight b2) 8)      
(test-eq "branch-weight" (branch-weight b3) 32)      

(test-eq "total weight" (total-weight sub) 40)      
(test-eq "total weight" (total-weight root) 42)      
      
(define (test-output)
  "test print routines"
  (let ((b1 (make-branch 1 2))
        (b2 (make-branch 4 8))
        (b3 (make-branch 16 32)))

    (define sub (make-mobile b2 b3))
    (define root (make-mobile b1 sub))
    (d "sub: ")
    (dnl sub)
    (print-mobile sub)
    (d "root: ")
    (dnl root)
    (print-mobile root)))

;(test-output)
(define b1 (make-branch 2 4))           ; torque 8 
(define b2 (make-branch 1 8))           ; torque 8
(define b3 (make-branch 4 9))           ; torque 36

(define sub (make-mobile b1 b2))        ; total weight 12
(define balanced (make-mobile b3 (make-branch 3 sub)))
(define un-balanced (make-mobile b2 (make-branch 2 sub)))

(test-eq "balanced" (mobile-balanced? balanced) #t)
(test-eq "un-balanced" (mobile-balanced? un-balanced) #f)
