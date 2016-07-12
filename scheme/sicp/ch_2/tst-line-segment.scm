;;;; Test suite for line-segment.scm
(load "test-framework.scm")
(load "line-segment.scm")
(load "point.scm")

(define seg0 (make-lseg (make-point 0 0) (make-point 2 2)))

(define (output-test)
  (print-lseg seg0)
  (newline))

(test-eq "start of segment"
  (start-segment seg0)
  (make-point 0 0))

(test-eq "end of segment"
  (end-segment seg0)
  (make-point 2 2))

(test-eq "midpoint"
  (midpoint-segment seg0)
  (make-point 1 1))

(define seg1 (make-lseg (make-point -2 -2) (make-point 1 2)))
(test-eq "midpoint, negative"
  (midpoint-segment seg1)
  (make-point -1/2 0))

(define seg2 (make-lseg (make-point 0 0) (make-point 3 4)))
(test-feq "length"
  (length-segment seg2)
  5)
