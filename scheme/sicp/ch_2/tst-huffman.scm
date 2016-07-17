;;;; tests for huffman.scm
(load "../test-framework.scm")
(load "huffman.scm")
(load "../lib.scm")

;;;; Simple data tests

(define test-section "simple: ")

(define simple-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-leaf 'B 2)))

(define simple-message '(A B B A))
(define simple-code '(0 1 1 0))

(test-eq "decoded" simple-message (decode simple-code simple-tree))
(test-eq "encoded" simple-code (encode simple-message simple-tree))

(test-eq "gen tree" (generate-huffman-tree '((A 4) (B 2))) simple-tree)

;;;; Avg data tests

(define test-section "avg: ")

(define avg-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-leaf 'C 1))))

(define avg-message '(A B C B A))
(define avg-code '(0 1 0 1 1 1 0 0))

(test-eq "decoded" avg-message (decode avg-code avg-tree))
(test-eq "encoded" avg-code (encode avg-message avg-tree))

(test-eq "gen tree" (generate-huffman-tree '((A 4) (B 2) (C 1))) avg-tree)

;;;; Complex data tests

(define test-section "complex: ")

(define complex-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define complex-message '(A D A B B C A))
(define complex-code '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(test-eq "decoded" complex-message (decode complex-code complex-tree))
(test-eq "encoded" complex-code (encode complex-message complex-tree))

(test-eq "gen tree" (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))) complex-tree)

;;;; Exercise 2.70

(define test-section "exercise 2.70: ")

(define song-tree
  (generate-huffman-tree '((na 16) (yip 9) (sha 3) (a 2) (get 2) (job 2) (boom 1) (wah 1))))

(define song-message
  '(get a job sha na na na na na na na get a job sha na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

(define song-encoded (encode song-message song-tree))

(test-eq "decoded" song-message (decode song-encoded song-tree))



