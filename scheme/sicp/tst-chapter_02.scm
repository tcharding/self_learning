;;;; tests for chapter_02.scm
(load "test-framework.scm")
(load "chapter_02.scm")
(load "lib.scm")
      
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

(test-eq "square-tree"
  (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
  '(1 (4 (9 16) 25) (36 49)))

;; accumulate-n defined in signal.scm
(load "signal.scm")
(test-eq "accumulate-n"
  (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
  '(22 26 30))
         
;;;; test exercise 2.54

(test-eq "*list" (list? '()) #t)
(test-eq "*list" (list? '(3)) #t)
(test-eq "*list" (list? (list 'a 'b)) #t)

(test-eq "*list" (list? 'aoue) #f)
(test-eq "*list" (list? 3) #f)

(test-eq "*symbol" (symbol? '()) #f)
(test-eq "*symbol" (symbol? '(3)) #f)
(test-eq "*symbol" (symbol? 3) #f)

(test-eq "*symbol" (symbol? 'aoue) #t)

(test-eq "*equals?" (*equal? 'a 'a) #t)
(test-eq "*equals?" (*equal? '() '()) #t)
(test-eq "*equals?" (*equal? '(()) '(())) #t)
(test-eq "*equals?" (*equal? '(3) '(3)) #t)
(test-eq "*equals?" (*equal? '(3 (2)) '(3 (2))) #t)
(test-eq "*equals?" (*equal? 3 3) #t)

(test-eq "*equals?" (*equal? '(3) '(0)) #f)
(test-eq "*equals?" (*equal? 3 2) #f)
(test-eq "*equals?" (*equal? 'aoeu 'stnh) #f)
