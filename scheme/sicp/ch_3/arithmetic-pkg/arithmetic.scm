;;;; Generic Arithmetic System
;;;;
;;;; Incorporates Exercises 2.74 - 2.93

(load-from-path "arithmetic-pkg/table.scm")
(load-from-path "arithmetic-pkg/integer.scm")
(load-from-path "arithmetic-pkg/rational.scm")
(load-from-path "arithmetic-pkg/real.scm")
(load-from-path "arithmetic-pkg/complex.scm")
(load-from-path "arithmetic-pkg/poly.scm")

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
(install-polynomial-package)
  
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
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

#!
(define (apply-generic op . args)
  "Raise types if process not found"
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((raised (raise-lowest args))) 
            (if raised
                (apply apply-generic (cons op raised))
                (error "No method for these types -- APPLY-GENERIC"
                       (list op type-tags))))))))
!#
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

(define (lt x y)
  (apply-generic 'lt x y))

(define (gt x y)
  (apply-generic 'gt x y))

(define (gcd x y)
  (apply-generic 'greatest-common-divisor x y))

(define (num-expt b e)
  (apply-generic 'exponential b e))

(define (reduce x y)
  (apply-generic 'reduce x y))

(define (maximum x y)
  (apply-generic 'maximum x y))

(define (minimum x y)
  (apply-generic 'minimum x y))

(define (*abs x)
  (apply-generic 'absolute x))

(define (*remainder x y)
  (apply-generic 'remainder x y))

(define (*quotient x y)
  (apply-generic 'quotient x y))

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
      (apply-generic 'real-part z)
      (error "arg is not a complex -- REAL-PART" x)))

(define (imag-part z)
  (if (complex-type? z)
      (apply-generic 'imag-part z)
      (error "arg is not a complex -- IMAG-PART" x)))

(define (complex-type? x)
  (or (complex? x)
      (rectangular? x)
      (polar? x)))

(define (all-complex-types? ls)
  (if (null? ls)
      #t
      (and (complex-type? (car ls))
           (all-complex-types? (cdr ls)))))

;;; Real Numbers

(define (make-real n)
  ((get 'make 'real) n))

;;; Polynomials

(define (make-poly var termlist)
  ((get 'make 'polynomial) var termlist))
         
;;; Miscellaneous Procedures

(define (square x)
  (* x x))

(define (equal-float? x y)
  "equal to three decimal place"
  (= (round (* 1000 x))
     (round (* 1000 y))))

;;; Tower of Types

(define (raise-lowest ls)
  "raise lowest type within ls"
  (let ((raised (raise-in-list integer? ls)))
    (if raised
        raised
        (let ((raised (raise-in-list rational? ls)))
          (if raised
              raised
              (let ((raised (raise-in-list real? ls)))
                (if raised
                    raised
                    #f)))))))

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
        ((real? x) (raise-real x))
        (else x)))

(define (raise-integer x)
  (make-rational (contents x) 1))

(define (raise-rational x)
  (make-real (div (numer x) (denom x))))

(define (raise-real x)
  (make-from-real-imag (contents x) 0))

(define (project x)
  (cond ((integer? x) #f)
        ((rational? x) (drop-rational x))
        ((real? x) (drop-real x))
        (else (drop-complex x))))
         
(define (drop-rational x)
  (make-integer (numer x)))

(define (drop-real x)
  (make-integer (round (contents x))))

(define (drop-complex x)
  (make-real (real-part x)))

(define (drop x)
  (let ((dropped (project x)))
    (if dropped
        (let ((raised (raise dropped)))
          (if (num-eq? raised x)
              (drop dropped)))
        x)))
  
