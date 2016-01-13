; The Little Schemer
;  Chapter 1 - Toys

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define r
  (lambda ()
    (RESTART 1)))

