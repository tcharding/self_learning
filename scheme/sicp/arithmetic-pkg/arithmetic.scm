;;;; Generic Arithmetic System
;;;;
;;;; Incorporates Exercises 2.74 - 2.93

(load-from-path "arithmetic-pkg/table.scm")
(load-from-path "arithmetic-pkg/ordinary-numbers.scm")

;(load-from-path "arithmetic-pkg/rational.scm")
;(load-from-path "arithmetic-pkg/complex.scm")

(install-scheme-number-package)
;(install-rational-package)
;(install-complex-package)

;; Operation Table

(define operation-table (make-table eqv?))
(define get (operation-table 'lookup))
(define put (operation-table 'insert!))

;; Coercion Table

(define coercion-table (make-table eqv?))
(define get-coercion (coercion-table 'lookup))
(define put-coercion (coercion-table 'insert!))
  
;;; Tag's

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'scheme-number))
;      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))
;      (error "Bad tagged datum -- CONTENTS" datum)))

;;; Generics

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get (list op type-tags))))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

;;; Generic Arithmetic Operations

(define (add x y)
  (apply-generic 'add x y))

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

(define (num-eq? x y)
  (apply-generic 'num-eq? x y))

(define (=zero? x)
  (apply-generic '=zero? x))

;;; Ordinary Numbers

(define (make-scheme-number n)
  ((get '(make scheme-number)) n))
#!
;;; Rational Numbers

(define (make-rational n d)
  ((get '(make rational)) n d))

;;; Complex Numbers

(define (make-from-real-imag x y)
  ((get '(make-from-real-imag complex)) x y))

(define (make-from-mag-ang r a)
  ((get '(make-from-mag-ang complex)) r a))
!#
;;; Miscellaneous Procedures

(define (square x)
  (* x x))

