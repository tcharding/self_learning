;;;; stack implementation (page 52)
(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
	(cond
	 [(eqv? msg 'empty?) (null? ls)]
	 [(eqv? msg 'push!) (set! ls (cons (car args) ls))]
	 [(eqv? msg 'top) (car ls)]
	 [(eqv? msg 'pop!) (set! ls (cdr ls))]
	 [(eqv? msg 'ref) (ref-get ls (car args))]
	 [(eqv? msg 'set! (ref-set ls (car args) (car (cdr args))))]
	 [else "oops"])))))

(define ref-get
  (lambda (l n)
    (cond
     ((zero? n) (car l))
      (else (ref-get (- n 1) (cdr l))))))

(define ref-set
  ;; set index i of list l to value v
  (lambda (l i v)
    (display i)
    (display v)
    (cond
     ((zero? i) (set-car! l v))
      (else (ref-get (- i 1) (cdr l))))))

;;;; modify implementation to use case
;;
;; Doesn't work: case won't compare strings with eqv? for some reason
;; (define make-stack-case
;;   (lambda ()
;;     (let ([ls '()])
;;       (lambda (msg . args)
;; 	(case msg
;; 	  ('empty? (null? ls))
;; 	  ('push! (set! ls (cons (car args) ls)))
;; 	  ;; ('top (car ls))
;; 	  ;; ('pop! (set! ls (cdr ls)))
;; 	  (else "oops"))))))



