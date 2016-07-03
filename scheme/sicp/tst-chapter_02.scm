;;;; tests for chapter_02.scm
(load "test-framework.scm")
(load "chapter_02.scm")

(test-eq "one" ((one inc) 0) 1)

(test-eq "two" ((two inc) 0) 2)

(test-eq "last-pair" (last-pair '(1 2 3)) 3)

(test-eq "reverse" (reverse '(1 2 3)) '(3 2 1))

(test-eq "same-parity" (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
(test-eq "same-parity" (same-parity 2 3 4 5 6 7) '(2 4 6))

(test-eq "square-list" (square-list '(1 2 3)) '(1 4 9))

(define x '((1 2) (3 4)))
(test-eq "reverse list of lists" (reverse x) '((3 4) (1 2)))
(test-eq "deep-reverse" (deep-reverse x) '((4 3) (2 1)))

(test-eq "fringe" (fringe '((1 2) ((3 4) (5)))) '(1 2 3 4 5))
