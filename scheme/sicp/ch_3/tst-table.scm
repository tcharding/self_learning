;;;; tests for table.scm
(load-from-path "test-framework.scm")
(load-from-path "table.scm")
(load-from-path "lib.scm")

(set-test-section! "one dimensional table")

(define t (make-table))
(test-eq "empty lookup" (lookup-1d 'a t) #f)

(insert! 'a 1 t)
(test-eq "empty lookup" (lookup-1d 'a t) 1)
(test-eq "empty lookup" (lookup-1d 'b t) #f)

(set-test-section! "two dimensional table")

(define t (make-table))
(insert-2d! 'math '* 4 t)
(insert-2d! 'math '+ 6 t)
(insert-2d! 'letters 'x 3 t)
(insert-2d! 'letters 'y 5 t)

(test-eq "" (lookup-2d 'math '* t) 4)
(test-eq "" (lookup-2d 'math '- t) #f)
(test-eq "" (lookup-2d 'letters 'x t) 3)

(set-test-section! "2D procedural")

(define operation-table (make-table-proc-2d))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'math '* 4)
(put 'math '+ 6)
(put 'letters 'x 3)
(put 'letters 'y 5)

(test-eq "" (get 'math '*) 4)
(test-eq "" (get 'math '-) #f)
(test-eq "" (get 'letters 'x) 3)

(set-test-section! "1D procedural")

(define operation-table (make-table-proc-1d))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'a 1)
(put 'b 2)
(put 'c 3)

(test-eq "" (get 'a) 1)
(test-eq "" (get 'c) 3)
(test-eq "" (get 'd) #f)
