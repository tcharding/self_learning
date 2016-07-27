;;;; Code used for testing interpreters
;;;;
;;;; not meant to be loaded
;;;;

#!
;; cut and paste

(define count 0)
  
(define (id x)
  (set! count (+ 1 count))
  x)

(define (square x)
  (* x x))

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

;; test implementation of exercise 4.31

(define (f a (b lazy) c (d lazy-memo))
  (cond ((= a 0) 1)
        ((= a 1) d)))

;; (define count 0)
;; after this runs count should be 1
(define (memoize-me)
  (define (id x)
    (set! count (+ 1 count))
    x)
  (square (id 10)))

(f 0 (/ 1 0) 'ignore 'ignore)           ; test lazy evaluation
(f 1 'ignore 'ignore memoize-me)        ; test memoize 

(define x 1) 

(define (p (e lazy)) e x) 

(p (set! x (cons x '(2)))) 

;;; streams-lazy-interpreter

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
       
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr itmes)))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car itmes))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       itmes))

(define (add-lists list-1 list-2)
  (cond ((null? list-1) list-2)
        ((null? list-2) list-1)
        (else (cons (+ (car list-1) (car list-2))
                    (add-lists (cdr list-1) (cdr list-2))))))

(define ones (cons 1 ones))

(define integers (cons 1 (add-lists ones integers)))
                                         
!#
