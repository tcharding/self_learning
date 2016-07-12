;;;; tests for queens.scm
(load "test-framework.scm")
(load "queens.scm")

;;;; test Constructor

(test-eq "empty-board" (empty-board 4) '(-1 -1 -1 -1))
(define %empty-board (empty-board 4))

;;;; test Selectors

(test-eq "set-column-row"
  (set-column-row 2 3 %empty-board)
  '(-1 -1 3 -1))

(clear-column 3 %empty-board)
(test-eq "dummy clear"
  (clear-column 2 (empty-board 4))
  %empty-board)

(test-eq "clear-column"
  (clear-column 2 '(2 3 0 -1))
  '(2 3 -1 -1))

;;;;

(test-eq "adjoin-position"
  (adjoin-position 0 0 %empty-board)
  '(0 -1 -1 -1))

(test-eq "adjoin-position mid"
  (adjoin-position 2 3 %empty-board)
  '(-1 -1 3 -1))

#!
; should throw exception
(test-eq "adjoin-position mid"
  (adjoin-position 2 2 '(2 3 6 6))
  '(2 3 2 6))
!#

;;;; test rows-safe?
(test-eq "rs empty-board" (rows-safe? 3 %empty-board) #t)
(test-eq "rs diagonal" (rows-safe? 3 '(1 2 3 4)) #t)
(test-eq "rs first two same yes" (rows-safe? 3 '(1 1 3 4)) #t)
(test-eq "rs first two same no" (rows-safe? 1 '(1 1 3 4)) #f)
(test-eq "last two same" (rows-safe? 3 '(-1 -1 3 3)) #f)

;;;; test diagonals-safe?
(test-eq "ds dummy" (diagonals-safe? 1 %empty-board) #t)
(test-eq "ds not" (diagonals-safe? 1 '(0 1 -1 -1)) #f)
(test-eq "ds yes" (diagonals-safe? 3 '(1 3 0 2)) #t)
