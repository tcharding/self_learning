;; Library example implementation
;;
;; spl page 86
(require rnrs)

(library (grades)
  (export gpa->grade gpa distribution)
  (import (rnrs))

  (define in-range?
    (lambda (x n y)
      (and (>= n x) (< n y))))

  (define-syntax range-case
    (syntax-rules (- else)
      [(_ expr ((x - y) e1 e2 ...) ... [else ee1 ee2])
       (let ([tmp expr])
         (cond
          [(in-range? x tmp y) e1 e2 ...]
          ...
          [else ee1 ee2 ...]))]
      [(_ expr ((x - y e1 e2 ...) ...)
          (let ([tmp expr])
            (cond
             [(in-range? x tmp y) e1 e2 ...]
             ...)))]))

  (define letter->number
    (lambda (x)
      (case x
        [(a) 4.0]
        [(b) 3.0]
        [(c) 2.0]
        [(d) 1.0]
        [(f) 0.0]
        [else (assertion-violation 'grade "invalid letter" x)])))

  (define gpa->grade
    (lambda (x)
      (range-case x
        [(0.0 - 0.5) 'f]
        [(0.5 - 1.5) 'd]
        [(1.5 - 2.5) 'c]
        [(2.5 - 3.5) 'b]
        [else 'a])))

  (define-syntax gpa
    (syntax-rules ()
      [(_ g1 g2 ...)
       (let ([ls (map letter->number (rem* 'x '(g1 g2 ...)))])
         (/ (apply + ls) (length ls)))]))

  (define rem*
    ; remove all occurences of atom from ls
    (lambda (atom ls)
      (cond
       ((null? ls) '())
       ((eqv? atom (car ls)) (rem* atom (cdr ls)))
       (else (cons (car ls) (rem* atom (cdr ls)))))))

  (define-syntax distribution
    (syntax-rules ()
      [(_ g1 g2 ...)
       (get-distribution (list g1 g2 ...))]))

  (define get-distribution
    (lambda (ls)
      (let f ([dist '((0 a) (0 b) (0 c) (0 d) (0 f))] [ls ls]))
	(cond
	 ((null? ls) dist)
	 (else (f (add-grade-to-dist (car ls) dist) (cdr ls))))))
  
  (define add-grade-to-dist
					; add grade to dist (list of pairs ((a 0) (b 2) ... (f 0))                                        
    (lambda (grade dist)
      (case grade
	[(a) (inc-grade (car dist))]
	[(b) (inc-grade (cadr dist))]
	[(c) (inc-grade (caddr dist))]
	[(d) (inc-grade (cadddr dist))]
	[(f) (inc-grade (cadddr (cdr dist)))]
	[else (assertion-violation 'grade "invalid letter" grade)])))

  (define inc-grade
    (lambda (pair)
      (set-car! (cdr pair) (+ 1 (car (cdr pair))))))

  (define grade-num
    (lambda (pair)
      (car (cdr pair))))

  (define grade-let
    (lambda (pair)
      (car pair)))

  )




