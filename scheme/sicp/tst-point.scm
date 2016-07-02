;;;; Test suite for point.scm
(load "test-framework.scm")
(load "point.scm")

(define p0 (make-point 1 2))
(define p1 (make-point 3 5))
(define p2 (make-point -2 -2))

(define (output-test)
  (print-point p0)
  (newline))

(test-eq "x-point"
  (x-point p0)
  1)

(test-eq "y-point"
  (y-point p0)
  2)

(test-eq "add two points"
  (add-point p0 p1)
  (make-point 4 7))

(test-eq "add two points, one negative"
  (add-point p0 p2)
  (make-point -1 0))

(test-eq "sub two points"
  (sub-point p1 p0)
  (make-point 2 3))

(define p2 (make-point 4 6))
(test-eq "halve point"
  (halve-point p2)
  (make-point 2 3))

(define p3 (make-point -4 -6))
(test-eq "halve point"
  (halve-point p3)
  (make-point -2 -3))

