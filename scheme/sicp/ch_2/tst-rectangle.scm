;;;; Test suite for rectangle.scm
(load "test-framework.scm")
(load "rectangle.scm")
(load "point.scm")

(define r0 (make-rect
            (make-point 0 0)
            (make-point 0 3)
            (make-point 3 0)))

(define (output-test)
  (print-rect r0))
