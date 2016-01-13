; The Little Schemer
;  Chapter 6 - Shadows

(define times
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (plus n (times n (sub1 m)))))))

(define plus
  (lambda (n m)
    (cond
     ((zero? n) m)
     (else (add1 (plus m (sub1 n)))))))

(define exp
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (times n (exp n (sub1 m)))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? '+ (car nexp))
      (plus (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
     ((eq? 'x (car nexp))
      (times (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
     (else
      (exp (value (car (cdr nexp))) (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? '+ (operator nexp))
      (plus (1st-sub-exp nexp) (2nd-sub-exp nexp)))
     ((eq? 'x (operator nexp))
      (times (1st-sub-exp nexp) (2nd-sub-exp nexp)))
     (else
      (exp (1st-sub-exp nexp) (2nd-sub-exp nexp))))))
