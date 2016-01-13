; The Little Schemer
;  Chapter 7 - Friends and Relations

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (equal? (car lat) a)
               (member? a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else
      (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat)
                 (makeset (cdr lat)))))))

(define multi-rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((equal? a (car lat))
      (multi-rember a (cdr lat)))
     (else (cons (car lat)
                 (multi-rember a (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cons (car lat) (makeset (multi-rember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))

(define set-difference
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (set-difference (cdr set1) set2))
     (else (cons (car set1)
                 (set-difference (cdr set1) set2))))))

(define intersect-all
  (lambda (l-set)
    (cond
     ((null? l-set) '())
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set) (intersect-all (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 '()))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
                 (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (second (first rel))
                        (first (first rel)))
                 (revrel (cdr rel)))))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))
      
(define fullfun?
  (lambda (rel)
     (fun? (revrel rel))))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     ((null? (cdr (car l))) '())
     (else (cons (second (first l))
                 (seconds (cdr l)))))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))
