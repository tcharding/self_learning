; The Little Schemer
;  Chapter 4 - number games

;
; We only consider positive numbers
;

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

(define minus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (minus n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (plus (car tup) (addtup (cdr tup)))))))

(define times
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (plus n (times n (sub1 m)))))))

(define tup-plus
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (plus (car tup1) (car tup2))
                 (tup-plus (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (> (sub1 n) (sub1 m))))))

(define >=
  (lambda (n m)
    (cond
     ((zero? m) #t)
     ((zero? n) #f)
     (else (>= (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (< (sub1 n) (sub1 m))))))

(define <=
  (lambda (n m)
    (cond
     ((zero? n) #t)
     ((zero? m) #f)
     (else (<= (sub1 n) (sub1 m))))))

(define equal
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (= (sub1 n) (sub1 m))))))

(define equal
  (lambda (n m)
    (cond
     ((< n m) #f)
     ((> n m) #f)
     (else #t))))

(define exp
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (times n (exp n (sub1 m)))))))

(define quotent
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (quotent (minus n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat)
                 (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat))
      (cons (car lat)
            (all-nums (cdr lat))))            
     (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a b)
    (cond
     ((and (number? a) (number? b)) (= a b))
     ((or (number? a) (number? b)) #f)
     (else (eq? a b)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
     (eqan? n 1)))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))))))
