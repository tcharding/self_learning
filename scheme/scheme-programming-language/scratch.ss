(import (rnrs io simple (6)))

(define assertion-violation
  (lambda (label msg obj)
    (display label)
    (display ": ")
    (display msg)
    (display ": ")
    (display obj)))

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

(define histogram
  (lambda (out dist)
    (define out-line
      (lambda (pair)
        (put-datum out (car pair))
        (put-string out ": ")
        (put-stars out pair)
        (newline)))
    (for-each out-line dist)))

(define put-stars
  ; outputs n stars: (a 2) -> **
  (lambda (out pair)
    (let f ([n (car (cdr pair))])
      (if (> n 0)
          (begin (put-string out "*")
                 (f (- n 1)))))))


    
