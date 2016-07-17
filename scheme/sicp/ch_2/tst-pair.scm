;;;; Test suite for example representation of a pair
;;;;
;;;; Exercise 2.5
(load "../test-framework.scm")
(load "pair.scm")
(load "../lib.scm")

(define raw-data '((1 2) (3 6) (10 100)))

(define (test-basic x y)
  "test constructor and selectors"
  (let ((tobj (cons-pair x y)))
    (test-eq "car" (car-pair tobj) x)
    (test-eq "cdr" (cdr-pair tobj) y)))

;;;;
;;;; Test suite run when file loaded
;;;;

(for-each (lambda (ls)
            (test-basic (first ls) (second ls)))
          raw-data)
