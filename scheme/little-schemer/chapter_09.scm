; The Little Schemer
;  Chapter 9 - ... and Again, and Again, and Again,...

(define test-looking
  (lambda ()
    (and
     (equal? #t (looking 'caviar '(6 2 4 caviar 5 7 3)))
     (equal? #f (looking 'caviar '(6 2 grits caviar 5 7 3))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     ((equal? a sorn) #t)
     (else #f))))
      
(define pick
  (lambda (n lat)
    (cond
     ((one? n) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

(define one?
  (lambda (n)
    (zero? (sub1 n))))

(define eternity
  (lambda (x)
    (eternity x)))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 '()))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define length-of-pair*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (plus (length* (car pora))
                 (length* (cdr pora)))))))

(define will-stop?
  (lambda (f)
    (f '())))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (plus (times (weight* (first pora)) 2)
            (weight* (second pora)))))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
                  (shuffle (second pora)))))))

(define eternity
  (lambda (x)
    (eternity x)))

; length 0
((lambda (length)
   (lambda ()
     (cond
      ((nul? l) 0)
      (else (add1 (length (cdr l)))))))
 eternity)

; length <= 1
((lambda (f)
   (lambda (l)
     (cond
      ((nul? l) 0)
      (else (add1 (f (cdr l)))))))
 ((lambda (g)
   (lambda (l)
     (cond
      ((nul? l) 0)
      (else (add1 (g (cdr l)))))))
  eternity))

((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))
