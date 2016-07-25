;;;; Deque Implementation
;;;;
;;;; deque made up from elements (data next previous), deque is cons cell of
;;;; front and rear pointers to front and rear of the list of elements.
;;;; 

;;; Constructors

(define (make-deque)
  (cons '() '()))

;;; Selectors

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (one-item? deque)
  (and (not (empty-deque? deque))
       (eq? (front-ptr deque)
            (rear-ptr deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (data-element (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (data-element (rear-ptr deque))))

;;; Mutators

(define (front-insert-deque! deque data)
  (let ((new-el (make-element data)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-el)
           (set-rear-ptr! deque new-el)
           deque)
          (else
           (link-element-infront new-el (front-ptr deque))
           (set-front-ptr! deque new-el)
           deque))))

(define (rear-insert-deque! deque data)
  (let ((new-el (make-element data)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-el)
           (set-rear-ptr! deque new-el)
           deque)
          (else
           (link-element-inrear new-el (rear-ptr deque))
           (set-rear-ptr! deque new-el)
           deque))))

(define (front-delete-deque! deque)
    (cond ((empty-deque? deque)
           (error "FRONT-DELETE called with an empty deque" deque))
          ((one-item? deque)
           (set-front-ptr! deque '())
           (set-rear-ptr! deque '())
           deque)
          (else
           (set-front-ptr! deque (next-element (front-ptr deque)))
           (set-element-prev! (front-ptr deque) '())
           deque)))

(define (rear-delete-deque! deque)
    (cond ((empty-deque? deque)
           (error "REAR-DELETE called with an empty deque" deque))
          ((one-item? deque)
           (set-front-ptr! deque '())
           (set-rear-ptr! deque '())
           deque)
          (else
           (set-rear-ptr! deque (prev-element (rear-ptr deque)))
           (set-element-next! (rear-ptr deque) '())
           deque)))


;;; Element ADT

(define (make-element data)
  (list data '() '()))

(define (data-element el)
  (car el))

(define (next-element el)
  (cadr el))

(define (prev-element el)
  (caddr el))

(define (set-element-next! el next)
  (set-car! (cdr el) next))

(define (set-element-prev! el prev)
  (set-car! (cddr el) prev))

;;; Helpers

(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))
   
(define (link-element-infront el-new-front el-old-front)
  (set-element-next! el-new-front el-old-front)
  (set-element-prev! el-old-front el-new-front))

(define (link-element-inrear el-new-rear el-old-rear)
  (set-element-next! el-old-rear el-new-rear)
  (set-element-prev! el-new-rear el-old-rear))

