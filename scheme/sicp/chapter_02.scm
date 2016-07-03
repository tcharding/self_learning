;;;; Various exercises from chapter 2
;;;;

;;; Church Numerals
(define (zero f)
    (lambda (x) x))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
 
;;;; Exercise 2.6

(define (one f)
  (lambda (x)
    (f x)))

(define (two f)
  (lambda (x)
    (f (f x))))

;;;; Exercise 2.17

(define (last-pair ls)
  (if (null? (cdr ls))
      (car ls)
      (last-pair (cdr ls))))

;;;; Exercise 2.18

(define (reverse ls)
  (let fn ((ls ls) (rev '()))
    (if (null? ls)
        rev
        (fn (cdr ls) (cons (car ls) rev)))))

;;;; Exercise 2.19

(define (cc amount coin-values)
  "Count ways of making up amount from list of coins"
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;;;; Exercise 2.20

(define (same-parity initial . nums)
  (reverse
   (let fn ((nums nums) (same (list initial)))
     (cond ((null? nums) same)
           (else (if (is-same-parity initial (car nums))
                     (fn (cdr nums) (cons (car nums) same))
                     (fn (cdr nums) same)))))))

(define (is-same-parity n m)
  (or (and (even? n) (even? m))
      (and (odd? n) (odd? m))))

;;;; Exercise 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square x)
  (* x x))

(define (square-list items)
  (map square items))

;;;; Exercise 2.23

(define (for-each proc ls)
  (cond ((null? ls) #t)
        (else (begin
                (proc (car ls))
                (for-each proc (cdr ls))))))

;;;; Exercise 2.27

(define (deep-reverse ls)
  (let fn ((ls ls) (rev '()))
    (cond ((null? ls) rev)
          ((atom? (car ls)) (fn (cdr ls) (cons (car ls) rev)))
          (else (fn (cdr ls) (cons (deep-reverse (car ls)) rev))))))

(define (atom? x)
  (not (pair? x)))
                        
;;;; Exercise 2.28

(define (fringe ls)
  "flatten ls"
  (cond ((null? ls) '())
        ((atom? (car ls)) (cons (car ls) (fringe (cdr ls))))
        (else (append (fringe (car ls)) (fringe (cdr ls))))))
         
