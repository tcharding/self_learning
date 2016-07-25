;;;; Various exercises from chapter 3
;;;;
(load "lib.scm")

;;;; Exercise 3.1

(define (make-accumulator initial-value)
  (lambda (n)
    (set! initial-value (+ initial-value n))
    initial-value))

;;;; Exercise 3.2

(define make-monitored
  (let ((count 0))
    (lambda (fn)
      (lambda (x)
        (cond ((eq? x 'how-many-calls) count)
              ((eq? x 'reset) (set! count 0))
              (else
               (set! count (inc count))
               (fn x)))))))

;;;; Exercise 3.3

(define (make-account acc-password balance)
  (let ((incorrect-password-attempts 0))
    (define %allowed-attempts 3)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password ignored-amount)
      (set! incorrect-password-attempts (inc incorrect-password-attempts))
      (if (>= incorrect-password-attempts %allowed-attempts)
          "Too many incorrect password attempts, please contact your local overlord"
          "Incorrect password"))
    (define (dispatch password message)
      (if (eq? password acc-password)
          (begin
            (set! incorrect-password-attempts 0)
            (cond ((eq? message 'withdraw) withdraw)
                  ((eq? message 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          incorrect-password))
      dispatch))

;;;; Exercise 3.6
(define rand-max 1000)
(define rand-init 0)

(define rand
  (let ((x rand-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (let ((tmp x))
               (set! x (rand-update x))
               tmp))
            ((eq? m 'reset)
             (lambda (seed)
               (set! x seed)))
            (else (error "Unknown request -- RAND" m))))))

;; appropriately chosen integers
(define a 1103515245)
(define b 12345)
(define m (expt 2 32))

(define (rand-update x)
  (mod (+ (* a x) b)
       m))

;;;;

(define (rng seed)
  "random number generator with seed"
  (let ((x seed))
    (lambda ()
      (let ((tmp x))
        (set! x (rand-update x))
        tmp))))

;;;; Exercise 3.7

(define (make-account acc-password balance)
  (let ((incorrect-password-attempts 0) (passwords (list acc-password)))
    (define %allowed-attempts 3)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password ignored-amount)
      (set! incorrect-password-attempts (inc incorrect-password-attempts))
      (if (>= incorrect-password-attempts %allowed-attempts)
          "Too many incorrect password attempts, please contact your local overlord"
          "Incorrect password"))
    (define (add-password extra-password)
      (set-cdr! passwords (list extra-password)))
    (define (dispatch password message)
      (if (member? password passwords)
          (begin
            (set! incorrect-password-attempts 0)
            (cond ((eq? message 'withdraw) withdraw)
                  ((eq? message 'deposit) deposit)
                  ((eq? message 'add-password) add-password)
                  (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          incorrect-password))
      dispatch))

(define (make-joint account acc-password extra-password)
  ((account acc-password 'add-password) extra-password)
  account)

;;;; Exercise 3.8

(define f-global 0)

(define (f n)
  (let ((tmp f-global))
    (set! f-global n)
    tmp))
            
;;;; Exercise 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;;;; count number of pairs in an s-exp

; http://codereview.stackexchange.com/questions/2497/correctly-count-the-number-of-pairs-in-an-irregular-list-structure

(define (count-pairs x)
  (define (iter x sum already-counted)
    (if (or (not (pair? x))
            (member x already-counted))
        (list sum already-counted)
        (let ((car-result (iter (car x) (+ sum 1) (cons x already-counted))))
          (iter (cdr x) (car car-result)
                (cadr car-result)))))
  (car (iter x 0 '())))


;;;; Exercise 3.18

(define (cycle? x)
  (define (iter x already-seen)
    (cond ((null? x) #f)
          ((atom? (car x))
           (or (member? x already-seen)
               (iter (cdr x) (cons x already-seen))))
          (else
           (or (cycle? (car x))
               (cycle? (cdr x))))))
  (iter x '()))


#!
;;; Exercise 3.50

(define (stream-map proc . argstreams)
  (if ((stream-null? argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))
!#

;;; Exercise 3.51

(define (show x)
  (dnl x)
  x)

(define (stream-enumerate-interval low high)
  (if (> low high)
      '()
      (stream-cons
       low
       (stream-enumerate-interval (inc low) high))))

(define x (stream-map show (stream-enumerate-interval 1 10)))

;;;; Exercise 3.52

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

;;;; Exercise 3.59

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1) (integrate-series (scale-stream sine-series -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
