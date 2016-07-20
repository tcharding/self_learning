;;;; Complex packages

;;; Rectangular Complex Number Package

(define (rectangular? x)
  (eq? (type-tag x) 'rectangular))
   
(define (install-rectangular-package)

  ;; internal procedures
  
  (define (make-from-real-imag x y)
    (cons x y))

  (define (make-from-mag-ang r a)
    (make-from-real-imag
     (* r (cos a))
     (* r (sin a))))

  (define (real-part z)
    (car z))
  
  (define (imag-part z)
    (cdr z))
  
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z)
          (real-part z)))

  (define (=zero? z)
    (and (equal-float? (real-part z) 0)
         (equal-float? (imag-part z) 0)))

  ;; interface to rest of system
  
  (define (tag x) (attach-tag 'rectangular x))
  
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put '=zero? '(rectangular) =zero?)

  (put 'make-from-real-imag
       'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang
       'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))

  'done)

;;; Polar Complex Number Package

(define (polar? x)
  (eq? (type-tag x) 'polar))

(define (install-polar-package)

  ;; internal procedures

  (define (magnitude z)
    (car z))
  
  (define (angle z)
    (cdr z))
  
  (define (make-from-mag-ang r a)
    (cons r a))
  
  (define (real-part z)
    (* (magnitude z)
       (cos (angle z))))
  
  (define (imag-part z)
    (* (magnitude z)
       (sin (angle z))))
  
  (define (=zero? z)
    (equal-float? (magnitude z) 0))
  
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to rest of system
  
  (define (tag x) (attach-tag 'polar x))
  
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put '=zero? '(polar) =zero?)
  
  (put 'make-from-real-imag
       'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  
  (put 'make-from-mag-ang
       'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  
  'done)

