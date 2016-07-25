;;;; Number System
;;;;
;;;; Incorporates Exercises 2.77 - 2.93
(load-from-path "complex.scm")

;;; Table of operations for the number system

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup op type)
      (let ((subtable (assoc op (cdr local-table))))
        (if subtable
            (let ((record (assoc type (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! op type item)
      (let ((subtable (assoc op (cdr local-table))))
        (if subtable
            (let ((record (assoc type (cdr subtable))))
              (if record
                  (set-cdr! record item)
                  (set-cdr! subtable
                            (cons (cons type item)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list op
                                  (cons type item))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define operation-table (make-table))
(define get (operation-table 'lookup))
(define put (operation-table 'insert!))

;;; Tag's

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;;; Generics

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

;;; Complex Numbers

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (angle z)
  (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(install-rectangular-package)
(install-polar-package)


;;; Miscellaneous Procedures

(define (square x)
  (* x x))

