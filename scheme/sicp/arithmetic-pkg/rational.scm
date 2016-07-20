;;;; Rational Number Package

(load-from-path "lib.scm")

(define (rational? x)
  (eq? (type-tag x) 'rational))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  
  (define (make-rat n d)
    (let ((r (reduce n d)))
;      (debug "make-rat: let" r)
      (cons (car r) (cadr r))))
#!
;; We can't use this when using rational functions
    (define (make-rat-pos n d)
      (let ((g (gcd n d)))    
        (cons (/ n  g) (/ d g))))
    (let ((rat (make-rat-pos (abs n) (abs d))))
      (cond ((and (negative? n) (negative? d)) rat)
            ((or (negative? n) (negative? d))
             (set-car! rat (sub 0 (car rat)))
             rat)
            (else rat))))po
!#
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (equal-rat? x y)
    (num-eq? (mul (numer x) (denom y))
             (mul (numer y) (denom x))))

  (define (zero-rat? x)
    (num-eq? (numer x) 0))

  (define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline))

  (define (negate-rat x)
    (make-rat (sub 0 (numer x)) (denom x)))             

  (define (negative-rat? x)
    (cond ((and (negative? (numer x)) (negative? (denom x))) #f)
          ((or (negative? (numer x)) (negative? (denom x))) #t)
          (else #f)))

  ;; interface to rest of system
  (define (tag x) (attach-tag 'rational x))

  (put 'denom
       '(rational)
       (lambda (x)
         (denom (contents x))))

  (put 'numer
       '(rational)
       (lambda (x)
         (numer (contents x))))

  (put 'add
       '(rational rational)
       (lambda (x y)
         (tag (add-rat x y))))

  (put 'sub
       '(rational rational)
       (lambda (x y)
         (tag (sub-rat x y))))

  (put 'mul
       '(rational rational)
       (lambda (x y)
         (tag (mul-rat x y))))

  (put 'div
       '(rational rational)
       (lambda (x y)
         (tag (div-rat x y))))

  (put 'num-eq?
       '(rational rational)
       (lambda (x y) (equal-rat? x y)))

  (put 'make
       'rational
       (lambda (n d)
         (tag (make-rat n d))))

  (put '=zero? '(rational) zero-rat?)
  'done)
