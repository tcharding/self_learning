; The Little Schemer
;  Chapter 8 - Lambda the Ultimate

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else (cons (car l)
                 (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? a (car l)) (cdr l))
       (else (cons (car l)
                   ((rember-f test?) a (cdr l))))))))

(define insert*
  (lambda (test? build-f)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((atom? (car l)) (cond
                         ((test? old (car l))
                          (build-f new old ((insert* test? build-f) new old (cdr l))))
                         (else (cons (car l)
                                     ((insert* test? build-f) new old (cdr l))))))
       (else (cons ((insert* test? build-f) new old (car l))
                   ((insert* test? build-f) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((atom? (car l)) (cond
                         ((equal? old (car l))
                          (seq new old ((insert-g seq) new old (cdr l))))
                         (else (cons (car l)
                                     ((insert-g seq) new old (cdr l))))))
       (else (cons ((insert-g seq) new old (car l))
                   ((insert-g seq) new old (cdr l))))))))

;(define insertL
;  (insert-g seqL))
(define insertL                     ; no need to define seqL, use lambda instead
  (insert-g (lambda (new old l)
              (cons new (cons old l)))))

(define insertR
  (insert-g (lambda (new old l)
              (cons old (cons new l)))))

(define subst
  (insert-g (lambda (new old l)
              (cons new l))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (n m)
    (cond
     ((zero? n) m)
     (else (add1 (plus m (sub1 n)))))))


(define times
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (plus n (times n (sub1 m)))))))

(define exp
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (times n (exp n (sub1 m)))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function (operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) plus)
     ((eq? x 'x) times)
     (else exp))))

(define operator
  (lambda (p)
    (car p)))

(define 1st-sub-exp
  (lambda (p)
    (car (cdr p))))

(define 2nd-sub-exp
  (lambda (l)
    (car (cdr (cdr l)))))

(define multi-rember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? a (car lat))
        ((multi-rember-f test?) a (cdr lat)))
       (else (cons (car lat)
                   ((multi-rember-f test?) a (cdr lat))))))))

(define multi-rember-eq?
  (multi-rember-f eq?))

(define multi-rember-f
  (lambda (test?)
    (lambda (lat)
      (cond
       ((null? lat) '())
       ((test? (car lat))
        ((multi-rember-f test?) (cdr lat)))
       (else (cons (car lat)
                   ((multi-rember-f test?) (cdr lat))))))))

(define tuna-test?
  (lambda (a)
    (eq? 'tuna a)))

(define multi-rember*-f
  (lambda (test?)
    (lambda (lat)
      (cond
       ((null? lat) '())
       ((atom? (car lat)) (cond
                           ((test? (car lat))
                            ((multi-rember-f test?) (cdr lat)))
                           (else (cons (car lat)
                                       ((multi-rember-f test?) (cdr lat))))))
       (else (cons ((multi-rember*-f test?) (car lat))
                   ((multi-rember*-f test?) (cdr lat))))))))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a (cdr lat)
                     (lambda (newlat seen)
                       (col newlat (cons (car lat) seen)))))
     (else
      (multirember&co a (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
         (cons 'tuna seen))))

(define multi-insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((eq? (car lat) old) (seq new old ((multi-insert-g seq) new old (cdr lat))))
       (else (cons (car lat) ((multi-insert-g seq) new old (cdr lat))))))))

(define multi-insertL
  (multi-insert-g (lambda (new old lat)
                    (cons new (cons old lat)))))

(define multi-insertR
  (multi-insert-g (lambda (new old lat)
                    (cons old (cons new lat)))))

(define multi-insertLR
  (multi-insert-g (lambda (new old lat)
                    (cons new (cons old (cons new lat))))))

(define multi-insertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
            (cons oldL
                  (multi-insertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
            (cons new
                  (multi-insertLR new oldL oldR (cdr lat)))))
     (else (cons (car lat)
                 (multi-insertLR new oldL oldR (cdr lat)))))))

(define multi-insertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multi-insertLR new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons new (cons oldL newlat)) (add1 L) R))))
     ((eq? (car lat) oldR)
      (multi-insertLR new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons oldR (cons new newlat)) L (add1 R)))))
     (else (cons (car lat)
                 (multi-insertLR new oldL oldR (cdr lat)
                                 (lambda (newlat L R)
                        (col (cons (car lat) newlat) L R))))))))

(define equal
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (= (sub1 n) (sub1 m))))))

(define times
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (plus n (times n (sub1 m)))))))

(define quotent
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (quotent (minus n m) m))))))

(define minus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (minus n (sub1 m)))))))

(define even?
  (lambda (n)
    (equal (times (quotent n 2) 2) n)))

(define evens-only?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (cond
                       ((even? (car l)) (evens-only? (cdr l)))
                       (else #f)))
     (else (and (evens-only? (car l))
                (evens-only? (cdr l)))))))

(define test-evens-only*
  (lambda ()
    (and
     (equal? '() (evens-only* '()))
     (equal? '(2) (evens-only* '(1 2 3)))
     (equal? '((2) 4) (evens-only* '(1 (2 3) 4))))))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l)
      '())
     ((atom? (car l))
      (cond
       ((even? (car l))
        (cons (car l)
              (evens-only* (cdr l))))
       (else
        (evens-only* (cdr l)))))
     (else
      (cons (evens-only* (car l))
            (evens-only* (cdr l)))))))

(define collector
  (lambda (evens-list product-of-evens sum-of-odds)
    (cons sum-of-odds
          (cons product-of-evens evens-list))))

(define test-evens-only*&co
  (lambda ()
    (and
     (equal? '(2 4 () 2) (evens-only*&co '((1) 2 3) collector)))))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l)
      (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col (cons (car l) newl) (times p (car l)) s))))
       (else
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col newl p (plus (car l) s)))))))
     (else (combine
            (evens-only*&co (car l)
                            (lambda (newl p s)
                              (col newl p s)))
            (evens-only*&co (cdr l)
                            (lambda (newl p s)
                              (col newl p s))))))))

; combine results of two calls to evens-only*&co
(define combine
  (lambda (l1 l2)
    (cons (times (first l1) (first l2))
          (cons (plus (second l1) (second l2))
                (merge-lists (cdr (cdr (l1)))
                             (cdr (cdr (l2))))))))

(define merge-lists
  (lambda (l1 l2)
    (cond
     ((null? l1) l2)
     (else
      (cons (car l1)
            (merge-lists (cdr l1) l2))))))

              
          
