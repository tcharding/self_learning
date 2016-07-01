(define calc #f)
(let ()
  (define do-calc
    (lambda (ek expr)
      (cond
       [(number? expr) expr]
       [(and (list? expr) (= (length expr) 2))
        (let ([op (car expr)] [arg (cadr expr)])
          (case op
            [(minus) (unary-minus ek arg)]
            [else (complain ek "invalid operator" op)]))]
       [(and (list? expr) (= (length expr) 3))
        (let ([op (car expr)] [args (cdr expr)])
          (case op
            [(add) (apply-op ek + args)]
            [(sub) (apply-op ek - args)]
            [(mul) (apply-op ek * args)]
            [(div) (apply-op ek / args)]
            [(mod) (apply-op ek mod args)]
            [else (complain ek "invalid operator" op)]))]
       [else (complain ek "invalid expression" expr)])))
  (define apply-op
    (lambda (ek op args)
      (op (do-calc ek (car args)) (do-calc ek (cadr args)))))
  (define unary-minus
    (lambda (op arg)
      (- 0 arg)))
  (define mod
    (lambda (d q)
      (- d (* (quotient d q) q))))
  (define complain
    (lambda (ek msg expr)
      (ek (list msg expr))))
  (set! calc
        (lambda (expr)
          ; grab an error continuation
          (call/cc
           (lambda (ek)
             (do-calc ek expr))))))

  
            
        
