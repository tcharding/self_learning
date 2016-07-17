;;;; Test suite for example representation of car/cons
;;;;
;;;; Exercise 2.4
(load "../test-framework.scm")
(load "car-cons.scm")
(load "../lib.scm")

(define raw-data '((1 2) (3 6) (-1 2) (-3 0)))

(define (test-basic x y)
  "test constructor and selectors"
  (let ((tobj (*cons x y)))
    (test-eq "car" (*car tobj) x)
    (test-eq "cdr" (*cdr tobj) y)))

;;;;
;;;; Test suite run when file loaded
;;;;

(for-each (lambda (ls)
            (test-basic (first ls) (second ls)))
          raw-data)
