;;;; tests for dequeue.scm
(load-from-path "test-framework.scm")
(load-from-path "deque.scm")
(load-from-path "lib.scm")

(set-test-section! "element")
(define el (make-element '()))
(test-eq "null?" (data-element el) '())
(test-eq "null?" (next-element el) '())
(test-eq "null?" (prev-element el) '())

(define el (make-element 'payload))
(test-eq "null?" (data-element el) 'payload)

(define e (make-element 'e))
(define f (make-element 'f))
(define g (make-element 'g))

;; e -> f -> g
(set-element-next! e f)                 
(set-element-next! f g)
(set-element-prev! g f)
(set-element-prev! f e)

(test-eq "" (next-element f) g)

(set-test-section! "deque")

(define d (make-deque))
(test-eq "empty" (empty-deque? d) #t)

(front-insert-deque! d 'a)              ; (a)
(test-eq "" (empty-deque? d) #f)
(test-eq "" (front-deque d) 'a)
(test-eq "" (rear-deque d) 'a)

(front-insert-deque! d 'b)              ; (b a)
(test-eq "front" (front-deque d) 'b)
(test-eq "rear" (rear-deque d) 'a)


(rear-insert-deque! d 'c)               ; (b a c)
(test-eq "front" (front-deque d) 'b)
(test-eq "rear" (rear-deque d) 'c)

(front-delete-deque! d)                 ; (a c)
(test-eq "front" (front-deque d) 'a)
(test-eq "rear" (rear-deque d) 'c)

(rear-delete-deque! d)                  ; (a)
(test-eq "front" (front-deque d) 'a)
(test-eq "rear" (rear-deque d) 'a)
#!
(front-insert-deque! d 'b)              ; (b a)
(rear-insert-deque! d 'c)               ; (b a c)
(rear-delete-deque! d)                  ; (b a)

!#
