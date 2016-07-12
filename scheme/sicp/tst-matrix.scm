;;;; tests for matrix.scm
(load "test-framework.scm")
(load "matrix.scm")
(load "lib.scm")

(define v '(1 2 3))
(define w '(4 5 6))

(define m (list v w))                   ; ((1 2 3) (4 5 6))
(define n '((1 2) (3 4) (5 6)))

(test-eq "dot-product" (dot-product v w) 32)

(test-eq "matrix-*-vector"
  (matrix-*-vector m v)
  '((1 4 9) (4 10 18)))

(test-eq "transpose"
  (transpose m)
  '((1 4) (2 5) (3 6)))

(test-eq "matrix-*-matrix"
  (matrix-*-matrix m n)
  '((1 6 15) (8 20 36)))
