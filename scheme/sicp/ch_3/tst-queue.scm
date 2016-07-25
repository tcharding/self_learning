;;;; tests for queue.scm
(load-from-path "test-framework.scm")
(load-from-path "queue.scm")
(load-from-path "lib.scm")

;;; test initial implementation
#!
(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)

(set-test-section! "queue")
(display "should print (a b):")
(newline)
(print-queue q)
!#

;;; test procedural implementation

(set-test-section! "procedural queue")

(define q (make-queue))
(test-eq "empty" (q 'empty?) #t)

((q 'insert!) 'a)
(display "printing queue after insert of 'a")
(newline)
(q 'print)
(test-eq "inserted a" (q 'front) 'a)
 
