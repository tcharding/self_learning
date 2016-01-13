; The Little Schemer
;  Chapter 5 - *Oh My Gawd*: It's Full of Stars

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define eqan?
  (lambda (a b)
    (cond
     ((and (number? a) (number? b)) (= a b))
     ((or (number? a) (number? b)) #f)
     (else (eq? a b)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eqan? a (car l)) (rember* a (cdr l)))
                       (else (cons (car l)
                                   (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eqan? old (car l)) (cons old
                                                  (cons new
                                                        (insertR* new old (cdr l)))))
                       (else (cons (car l)
                                   (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l))
                 (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l)) (cond
                       ((eqan? a (car l)) (add1 (occur* a (cdr l))))
                       (else (occur* a (cdr l)))))
     (else (+ (occur* a (car l))
                 (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eqan? old (car l)) (cons new
                                                  (subst* new old (cdr l))))
                       (else (cons (car l)
                                   (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l))
                 (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eqan? old (car l)) (cons new
                                                  (cons old (insertL* new old (cdr l)))))
                       (else (cons (car l)
                                   (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l))
                 (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (cond
                       ((eqan? a (car l)) #t)
                       (else (member* a (cdr l)))))
     (else (or (member* a (car l))
               (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((atom? (car l1)) (cond
                        ((atom? (car l2)) (cond
                                           ((eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
                                           (else #f)))
                        (else #f)))
     (else (cond
            ((atom? (car l2)) #f)
            (else (and (eqlist? (car l1) (car l2))
                       (eqlist? (cdr l1) (cdr l2)))))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
                 (rember s (cdr l)))))))
