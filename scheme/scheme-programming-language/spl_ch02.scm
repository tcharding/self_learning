;; 2.4.3 remove shadowing
; a original
(let ([x 'a] [y 'b])
  (list (let ([x 'c]) (cons x y))
        (let ([y 'd]) (cons x y))))
; a solution
(let ([x 'a] [y 'b])
  (list (let ([xc 'c]) (cons xc y))
        (let ([yd 'd]) (cons x yd))))

; b original
(let ([x '((a b) c)])
  (cons (let ([x (cdr x)])
	  (car x))
	(let ([x (car x)])
	  (cons (let ([x (cdr x)])
		  (car x))
		(cons (let ([x (car x)])
			x)
		      (cdr x))))))
; b solution
(let ([x1 '((a b) c)])
  (cons (let ([x2 (cdr x1)])
	  (car x2))
	(let ([x3 (car x1)])
	  (cons (let ([x4 (cdr x3)])
		  (car x4))
		(cons (let ([x5 (car x3)])
			x5)
		      (cdr x3))))))

(define compose
  (lambda (p1 p2)
    (lambda (x)
      (p1 (p2 x)))))

(define cadr (compose car cdr))
(define cddr (compose cdr cdr))

(define caadr (compose car cadr))
(define caddr (compose car cddr))
(define cdddr (compose cdr cddr))

(define caaadr (compose car cadr))
(define caaddr (compose car caddr))
(define cadddr (compose car cdddr))
(define cddddr (compose cdr cdddr))

;; return the shorter of two lists
;;  returns first list if same length
(define shorter
  (lambda (l1 l2)
    (if (< (length l2) (length l1))
	l2
	l1)))

(define length
  (lambda (l)
    (if (null? l)
	0
	(+ 1 (length (cdr l))))))

;; return n'th item from l
(define list-ref
  (lambda (l n)
    (cond
     ((zero? n) (car l))
     (else (list-ref l (- n 1))))))

;; return tail of l starting with index n
(define list-tail
  (lambda (l n)
    (cond
     ((zero? n) l)
     (else (list-tail (cdr l) (- n 1))))))

;; return shorter of two lists, first if same length
(define shorter
  (lambda (l1 l2)
    (if (shorter? l1 l2)
	l1
	l2)))

(define shorter?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((null? l1) #t)
     ((null? l2) #f)
     (else (shorter? (cdr l1) (cdr l2))))))

(define odd?
  (lambda (n)
    (cond
     ((= 0 n) #f)
     (else (even? (- n 1))))))

(define even?
  (lambda (n)
    (cond
     ((zero? n) #t)
     (else (odd? (- n 1))))))

;; lazy evaluation (and thunks) example. See page 51
(define lazy
  (lambda (t)
    (let ([val #f] [flag #f])
      (lambda ()
	(if (not flag)
	    (begin (set! val (t))
		   (set! flag #t)))
	val))))

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define cycles
  ; hare and turtle cycle detection algorithm
  (lambda (h t)
    (cond
     ((null? h) #f)
     ((equ? h t) #t)
     (else (cycles (cdr (cdr h) (cdr h)))))))

; handle all types of input including pairs, and cyclic lists
(define list?
  (lambda (sexp)
    (cond
     ((atom? sexp) #f)
     ((pair? sexp)
      (if (atom? (cdr sexp)) #f))
     ; check for cycles
     (
     (else #t))))
