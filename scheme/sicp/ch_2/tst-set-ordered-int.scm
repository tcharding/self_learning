;;;; tests for set-ordered-int.scm
(load "../test-framework.scm")
(load "set-ordered-int.scm")
(load "../lib.scm")

;;;
(set! test-section "element-of-set?: ")

(test-eq "simple #t" (element-of-set? '1 '(1 2 3)) #t)
(test-eq "simple #f" (element-of-set? '1 '(2 3)) #f)

;;;
(set! test-section "equal-set?: ")

(test-eq "#t"
  (equal-set? '(1 2 3)
              '(1 2 3))
  #t)

(test-eq "#f"
  (equal-set? '(1 2 3 4)
              '(1 2 3))
  #f)

;;;
(set! test-section "adjoin-set: ")

(define s '(1 3 6))
(define adjoined (adjoin-set 1 s))
(test-eq "no add" (equal-set? s adjoined) #t)

(define adjoined (adjoin-set 2 s))
(test-eq "add #f" (equal-set? s adjoined) #f)
(test-eq "add #t" (equal-set? adjoined '(1 2 3 6)) #t)

;;;
(set! test-section "intersection-set: ")

(define p '(1 3 6))
(define q '(7 8 9))
(test-eq "empty" (intersection-set p q) '())

(define p '(1 3 6))
(define q '(3 6 7 8 9))
(test-eq "3 6" (intersection-set p q) '(3 6))

;;;
(set! test-section "union-set: ")

(define p '(1 3 6))
(define q '(7 8 9))
(test-eq "empty" (union-set p q) '(1 3 6 7 8 9))

(define p '(1 3 6))
(define q '(3 6 7 8 9))
(test-eq "3 6" (union-set p q) '(1 3 6 7 8 9))
