;;;; tests for queens.scm
(load "../test-framework.scm")
(load "queens.scm")

(test-eq "empty board"
  empty-board
  '())

(test-eq "adjoin to empty"
  (adjoin-position 1 1 '())
  '(1))

(test-eq "adjoin to one pos"
  (adjoin-position 2 2 '(1))
  '(1 2))

(test-eq "adjoin to two pos"
  (adjoin-position 2 2 '(1 2))
  '(1 2 2))

(test-eq "contains-duplicates no"
  (contains-duplicates? '(1 2 3 4))
  #f)

(test-eq "contains-duplicates yes"
  (contains-duplicates? '(1 2 3 4 3))
  #t)



  

