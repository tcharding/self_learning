;;;; library of Scheme utilities                                        

(define r
  (lambda ()
    (RESTART 1)))

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
