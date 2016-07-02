(define Ackermann
  ; Ackermann's function
  (lambda (x y)
    (cond
     ((= y 0) 0)
     ((= x 0) (* 2 y))
     ((= y 1) 2)
     (else (Ackermann (- x 1)
              (Ackermann x (- y 1)))))))

(define f
  (lambda (n)
    (Ackermann 0 n)))

(define g
  (lambda (n)
    (Ackermann 1 n)))

(define h
  (lambda (n)
    (Ackermann 2 n)))
