;;;; tests for chapter_03.scm
(load-from-path "test-framework.scm")
(load-from-path "chapter_03.scm")
(load-from-path "lib.scm")

(define acc (make-account 'secure 0))

;;; test count-pairs Exercise 3.16
(define test-section "count-pairs: ")
(test-eq "simple" (count-pairs '(a b c)) 3)

(test-eq "pair" (count-pairs (cons '(a) '(b))) 3)
(define p '(a b))
(define twin (cons p p))
(test-eq "wrong: 3 gets 5" (count-pairs twin) 3)
(test-eq "wrong: 3 gets 4" (count-pairs (cons p (cdr p))) 3)
(test-eq "wrong: 5 gets 7" (count-pairs (cons twin '(a))) 5)
(test-eq "4" (count-pairs (cons twin twin)) 4)

(define test-section "cycle?: ")

(test-eq "false" (cycle? '(a b c)) #f)
(define has-cycle '(a b c))
(set-cdr! has-cycle has-cycle)
(test-eq "true" (cycle? has-cycle) #t)
(test-eq "nested" (cycle? (cons has-cycle '(a b c))) #t)
