;;;; Section 2.3.3 Representing Sets

(load "../lib.scm")

;;; Represent a set as an un-ordered list without duplicates

(define (make-set ls)
  (cond ((null? ls) '())
        ((member? (car ls) (cdr ls))
         (make-set (cdr ls)))
        (else (cons (car ls) (make-set (cdr ls))))))

(define (element-of-set? obj set)
  (member? obj set))

(define (adjoin-set obj set)
  (if (element-of-set? obj set)
      set
      (cons obj set)))

(define (union-set p q)
  (cond ((null? p) q)
        ((null? q) p)
        ((element-of-set? (car p) q)
         (union-set (cdr p) q))
        (else (cons (car p) (union-set (cdr p) q)))))

(define (intersection-set p q)
  (cond ((or (null? p) (null? q)) '())
        ((element-of-set? (car p) q)
         (cons (car p) (intersection-set (cdr p) q)))
        (else (intersection-set (cdr p) q))))

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
  
;;; Represent a set as an un-ordered list with duplicates

(define (make-set ls)
  ls)

(define (element-of-set? obj set)
  (member? obj set))
         
(define (adjoin-set obj set)
  (cons obj set))

(define (union-set p q)
  (append p q))

(define (intersection-set p q)
  (cond ((or (null? p) (null? q)) '())
        ((element-of-set? (car p) q)
         (cons (car p) (intersection-set (cdr p) q)))
        (else (intersection-set (cdr p) q))))

(define (equal-set? p q)
  (and (and (set? p) (set? q))
       (and (subset? p q)
            (subset? q p))))

(define (set? p)
  (pair? p))

(define (subset? p q)
  (if (null? p)
      #t
      (and (element-of-set? (car p) q)
           (subset? (cdr p) q))))
