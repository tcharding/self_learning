(define square
  (lambda (n)
    (* n n)))

(define reciprocal
  (lambda (n)
    (if (= n 0)
        "oops!"
        (/ 1 n))))
