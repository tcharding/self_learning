; interpreter cannot refernce assertion-violation?
(define reciprocal
  (lambda (n)
    (if (and (number? n) (not (= n 0)))
        (/ 1 n)
        (assertion-violation 'reciprocal
                             "improper argument"
                             n))))
