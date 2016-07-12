;;;; Section 2.3.3 Representing Sets
;;;;

(define (equal-set? p q)
  (and (and (set? p) (set? q))
       (and (subset? p q)
            (subset? q p))))

(define (set? p)
  (if (null? p)
      #t
      (and (not (element-of-set? (car p) (cdr p)))
           (set? (cdr p)))))

(define (subset? p q)
  (if (null? p)
      #t
      (and (element-of-set? (car p) q)
           (subset? (cdr p) q))))
