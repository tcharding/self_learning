;;;; Exercise 4.21
(load-from-path "lib.scm")

(define (fib n)
  (cond ((= n 1) 1)
        ((= n 2) 1)
        (else (+ (my-fib (- n 1)) (my-fib (- n 2))))))

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)
                 
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fn k)
      (cond ((= k 1) 1)
            ((= k 2) 1)
            (else (+ (fn fn (- k 1)) (fn fn (- k 2))))))))
 10)                                     ; fib 10 = 55
    
(define (f x)
  "even?"
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         #t
         (od? od? ev? (dec n))))
   (lambda (ev? od? n)
     (if (= n 0)
         #f
         (ev? ev? od? (dec n))))))
             
   
(define (f x)          
  "even?"
  (define (even? n)
    (if (= n 0)
        #t
        (odd? (dec n))))

  (define (odd? n)
    (if (= n 0)
        #f
        (even? (dec n))))
  (even? x))

(define (fib n)
  "Lambda the great!"
  ((lambda (f)
     (f f n))
   (lambda (fn k)
     (cond ((= k 1) 1)
           ((= k 2) 1)
           (else (+ (fn fn (- k 1)) (fn fn (- k 2))))))))

