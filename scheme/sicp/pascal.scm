;;;; Compute pascals triangle upto row n

(define pascal
  (lambda (n)
    (let fn ((row '(1)) (n n))
      (unless (< n 0)
        (display row)
        (newline)
        (fn (next-row row) (- n 1))))))

(define next-row
  (lambda (row)
    (let fn ((row (cons 0 row)))
      (cond
       ((null? (cdr row)) '(1))
       (else (cons (+ (car row) (cadr row))
                   (fn (cdr row))))))))
               
