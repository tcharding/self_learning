;;;; Queue implementation, Section 3.3.2

;;; Constructor

(define (make-queue)
  (cons '() '()))

;;; Selectors

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" q)
      (car (front-ptr q))))

;;; Predicates

(define (empty-queue? q)
  (null? (front-ptr q)))

;;; Mutators

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
           (set-cdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))

(define (delete-queue? q)
  (cond ((empty-queue? q)
         (error "DELETE! called on empty queue" q))
        (else
         (set-front-ptr! q (cdr (front-ptr q)))
         q)))

;;; Helpers

(define (front-ptr q)
  (car q))

(define (rear-ptr q)
  (cdr q))

(define (set-front-ptr! q item)
  (set-car! q item))

(define (set-rear-ptr! q item)
  (set-cdr! q item))

;;; Queue procedures

(define (print-queue q)
  (let iter ((item (front-ptr q)))
    (if (null? item)
        (newline)
        (begin
          (display item)
          (display " ")))))
