; The Little Schemer
;  Chapter 2 - Do It, Do It Again, and Again, and Again ...

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define lat?
  (lambda (l)
    (if (null? l)
        #t
        (and (atom? (car l)) (lat? cdr l)))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))


