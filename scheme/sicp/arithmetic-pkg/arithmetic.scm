;;;; Generic Arithmetic System
;;;;
;;;; Incorporates Exercises 2.74 - 2.93

(load-from-path "arithmetic-pkg/table.scm")
(load-from-path "arithmetic-pkg/integer.scm")
(load-from-path "arithmetic-pkg/rational.scm")
(load-from-path "arithmetic-pkg/real.scm")
(load-from-path "arithmetic-pkg/complex.scm")

;; Coercion Table

(define coercion-table (make-table-2d))
(define get-coercion (coercion-table 'lookup))
(define put-coercion (coercion-table 'insert!))

;; Operation Table

(define operation-table (make-table-2d))
(define get (operation-table 'lookup))
(define put (operation-table 'insert!))

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
  
;;; Tag's

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'integer))
;      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))
;      (error "Bad tagged datum -- CONTENTS" datum)))

;;; Generics

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc
             (apply proc (map contents args)))
            ((not (all-complex-types? type-tags))
             (apply apply-generic (cons op (raise-lowest args))))
            (else
             (error "No method for these types -- APPLY-GENERIC" (list op type-tags)))))))

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

(define (make-integer n)
  ((get 'make 'integer) n))

;;; Rational Numbers

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (numer x)
  (if (rational? x)
      ((get 'numer '(rational)) x)
      (error "arg is not a rational -- NUMER" x)))

(define (denom x)
  (if (rational? x)
      ((get 'denom '(rational)) x)
      (error "arg is not a rational -- DENOM" x)))

;;; Complex Numbers

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z)
  (if (complex-type? z)
      ((get 'real-part '(complex)) z)
      (error "arg is not a complex -- REAL-PART" x)))

(define (imag-part z)
  (if (complex-type? z)
      ((get 'imag-part '(complex)) z)
      (error "arg is not a complex -- IMAG-PART" x)))

;;; Real Numbers

(define (make-real n)
  ((get 'make 'real) n))

;;; Miscellaneous Procedures

(define (square x)
  (* x x))

(define (complex-type? x)
  (or (complex? x)
      (rectangular? x)
      (polar? x)))

(define (all-complex-types? ls)
  (if (null? ls)
      #t
      (and (complex-type? (car ls))
           (all-complex-types? (cdr ls)))))
                          
;;; Tower of Types

(define (raise-lowest ls)
  "raise lowest type within ls"
  (let ((raised (raise-in-list integer? ls)))
    (if raised
        raised
        (let ((raised (raise-in-list rational? ls)))
          (if raised
              raised
              #f)))))

(define (raise-in-list predicate? ls)
  (let iter ((seen '()) (ls ls))
    (cond ((null? ls) #f)
          ((predicate? (car ls))
           (append seen (cons (raise (car ls)) (cdr ls))))
          (else
           (iter (append seen (list (car ls))) (cdr ls))))))

(define (raise x)
  (cond ((integer? x) (raise-integer x))
        ((rational? x) (raise-rational x))
        (else x)))

(define (raise-integerx x)
  (make-rational (contents x) 1))

(define (raise-rational x)
  (make-real (/ (numer x) (denom x))))

; we can't do this until complex numbers can handle rational parts!
;(define (raise-rational x)
;  (make-from-real-imag (contents x) 0))
         
