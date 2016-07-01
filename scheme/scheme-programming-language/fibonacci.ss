(define fibonacci
  (let ((counter 0))
    (lambda (n)
      (let fib ((i n))
        (set! counter (+ 1 counter))
        (cond
         ((= i -1) (begin (display "counter: ")
                          (display counter)
                          (newline)
                          (set! counter 0)))
         ((= i 0) 0)
         ((= i 1) 1)
         (else (+ (fib (- i 1)) (fib (- i 2)))))))))

(define counter 0)
(define fibonacci-tail
      (lambda (n)
        (if (= n 0)
            0
            (let fib ((i n) (a1 1) (a2 0))
              (set! counter (+ 1 counter))
              (if (= i 1)
                  a1
                  (fib (- i 1) (+ a1 a2) a1))))))
    
