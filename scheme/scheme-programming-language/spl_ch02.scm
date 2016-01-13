;; 2.4.3 remove shadowing
(let ([x 'a] [y 'b])
  (list (let ([x 'c]) (cons x y))
        (let ([y 'd]) (cons x y))))

(let ([x 'a] [y 'b])
  (list (let ([xc 'c]) (cons xc y))
        (let ([yd 'd]) (cons x yd))))
