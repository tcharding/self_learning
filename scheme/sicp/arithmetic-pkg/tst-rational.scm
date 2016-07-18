;;;; Test suite for rational.scm
(load-from-path "test-framework.scm")
(load-from-path"arithmetic-pkg/rational.scm")

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define one-quarter (make-rat 1 4))

(test-eq "add half and quarter"
  (add-rat one-half one-quarter)
  (make-rat 3 4))

(test-eq "sub quarter from half"
  (sub-rat one-half one-quarter)
  (make-rat 1 4))

(test-eq "mul half and quarter"
  (mul-rat one-half one-quarter)
  (make-rat 1 8))

(test-eq "div half and quarter"
  (div-rat one-half one-quarter)
  (make-rat 2 1))

(test-eq "make-rat two negatives"
  (make-rat -1 -2)
  (cons 1 2))

(test-eq "make-rat one negative"
  (make-rat -1 2)
  (cons -1 2))
