;;;; Queue implementation, Section 3.3.2

;;; Constructor

(define (make-queue)
  (cons '() '()))

;;; Selectors

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" q)
      (car (front-ptr q))))

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

;;;

(define (print-queue q)
  (display (front-ptr q))
  (newline))

;;;; Queue implemented as procedures

;;; HAS BUGS (IN INSERT!)
(define (make-queue)
  (let ((q (cons '() '())))
    (let ((front-ptr (car q))
          (rear-ptr (cdr q)))
      (define (set-front-ptr! item)
        (set-car! q item))
      (define (set-rear-ptr! item)
        (set-cdr! q item))
      (define (empty?)
        (null? front-ptr))
      (define (front)
        (cond ((empty?) '())
              (else (car front-queue))))
      (define (insert! item)
        (let ((new-pair (cons item '())))
          (cond ((empty?)
                 (set-front-ptr! new-pair)
                 (set-rear-ptr! new-pair))
                (else
                 (set-cdr! rear-ptr new-pair)
                 (set-rear-ptr! new-pair)))))
      (define (delete!)
        (cond ((empty?)
               (error "DELETE! called on empty queue" q))
              (else
               (set-front-ptr! (cdr (front-ptr))))))
      (define (print)
        (display front-ptr)
        (newline))
      (define (dispatch m)
        (cond 
         ((eq? m 'empty?) (empty?))
         ((eq? m 'front) (front))
         ((eq? m 'insert!) insert!)
         ((eq? m 'print) (print))
         (else
          (error "Undefined operation -- QUEUE" m))))
      dispatch)))

        
         
