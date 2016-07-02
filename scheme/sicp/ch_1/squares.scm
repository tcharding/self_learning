(define assertion-error
  (lambda (msg obj)
    (display msg)
    (display obj)))

(define sum-squares-largest-two
  ;; return the sum of the squares of the largest two numbers of list
  (lambda (ls)
    (sum-of-squares (two-largest ls))))

(define sum-of-squares
  (lambda (ls)
    (cond
     ((null? ls) 0)
     (else (+ (* (car ls) (car ls)) (sum-of-squares (cdr ls)))))))

(define two-largest
  ;; return the two largest numbers from list
  (lambda (ls)
    (let f ((n 0) (res '(0 0)) (ls ls))
      (cond
       ((null? ls) res)
       (else (f (car ls) (max-pair (car ls) res) (cdr ls)))))))

(define max-pair
  (lambda (n pair)
    (cond
     ((and (< n (car pair)) (< n (cadr pair)))
      pair)
     ((< n (car pair)) (cons n (cdr pair)))
     (else (cons n (cons (car pair) '()))))))

;(define (a-plus-abs-b a b)
;  ((if (> b 0) + -) a b))

(define a-plus-abs-b
  (lambda (a b)
    (
     (if (> b 0)
	 +
	 -)
     a b)))
