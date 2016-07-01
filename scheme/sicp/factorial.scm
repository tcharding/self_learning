;;;; Calculate factorial n

(define factorial-r
  (lambda (n)
    "recursive version"
    (cond
     ((= n 0) 1)
     (else (* n (factorial-r (- n 1)))))))

;; call iterative version
(define factorial
  (lambda (n)
    (factorial-iter n 1)))

;; define this at top level so we can trace factorial
(define factorial-iter
  (lambda (n a)
      (cond
       ((= n 0) a)
       (else (factorial-iter (- n 1) (* a n))))))
