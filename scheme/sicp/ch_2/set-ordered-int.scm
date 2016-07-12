;;;; Section 2.3.3 Representing Sets

;;; Represent a set as an ordered list without duplicates
;;;
;;; Only supports integers

;(define (make-set ls)
;  (ascending (remove-duplicates ls)))

(define (element-of-set? int set)
  (cond ((null? set) #f)
        ((= int (car set)) #t)
        ((< int (car set)) #f)
        (else (element-of-set? int (cdr set)))))

(define (adjoin-set int set)
  "insert int into set in correct place"
  (cond ((null? set) (list int))
        ((= int (car set)) set)
        ((< int (car set)) (cons int set))
        (else (cons (car set) (adjoin-set int (cdr set))))))

(define (union-set p q)
    (cond ((null? p) q)
          ((null? q) p)
          (else (let ((x1 (car p)) (x2 (car q)))
                  (cond ((= x1 x2) (cons x1 (union-set (cdr p) (cdr q))))
                        ((< x1 x2) (cons x1 (union-set (cdr p) q)))
                        (else (cons x2 (union-set p (cdr q)))))))))

(define (intersection-set p q)
  (cond ((or (null? p) (null? q)) '())
        ((element-of-set? (car p) q)
         (cons (car p) (intersection-set (cdr p) q)))
        (else (intersection-set (cdr p) q))))

(define (equal-set? p q)
  "p and q may or may not be sets"
  (define (equal-set-hlpr? p q)
    "p and q are sets"
    (cond ((and (null? p) (null? q)) #t)
          ((null? p) #f)
          ((null? q) #f)
          (else (and (= (car p) (car q))
                     (equal-set-hlpr? (cdr p) (cdr q))))))
  (and (and (set? p) (set? q))
       (equal-set-hlpr? p q)))

(define (set? ls)
  (and (ascending? ls)
       (not (contains-duplicates? ls))))

(define (subset? p q)
  (if (null? p)
      #t
      (and (element-of-set? (car p) q)
           (subset? (cdr p) q))))

(define (ascending? ls)
  (cond ((null? ls) #t)
        ((null? (cdr ls)) #t)
        (else (and (< (car ls) (cadr ls))
                   (ascending? (cdr ls))))))
         
(define (contains-duplicates? ls)
  (if (null? ls)
      #f
      (and (not (member? (car ls) (cdr ls)))
           (contains-duplicates? (cdr ls)))))
