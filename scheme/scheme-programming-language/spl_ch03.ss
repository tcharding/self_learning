; from text, page 71
(define factor-next
  (lambda (n)
    (let f ((n n) (i 2))
      (cond
       ((>= i n) (list n))
       ((integer? (/ n i))
        (cons i (f (/ n i) i)))
       (else (f n (+ i 1)))))))

; now using letrec (exercise 3.2.1)
(define factor
  (lambda (n)
    (letrec ((f (lambda (n i) 
                  (cond
                   ((>= i n) (list n))
                   ((integer? (/ n i))
                    (cons i (f (/ n i) i)))
                   (else (f n (+ i 1)))))))
      (f n 2))))

(letrec ((even?
          (lambda (x)
            (or (= x 0)
                (odd? (- x 1)))))
         (odd?
          (lambda (x)
            (and (= x 0)
                 (even? (- x 1))))))
  even? 19)

; exercise 3.2.3
(let even? ((odd?
             (lambda (x)
               (and (= x 0)
                    (even? (- x 1)))))
            (x x))
  (or (= x 0)
      (odd? (- x 1)))
  (even? 20))

;;;; section 3.3

(define product
  (lambda (l)
    (call/cc
     (lambda (break)
       (let f ([l l])
         (cond
          [(null? l) 1]
          [(= (car l) 0) (break 0)]
          [else (* (car l) (f (cdr l)))]))))))

;; infinite loop using call/cc
(define f #f)
(define n 0)

(define loop
 (lambda ()
   (call/cc
    (lambda (k)
      (set! f k)))
   (write n)
   (newline)
   (set! n (+ n 1))
   (f "ignore")))

