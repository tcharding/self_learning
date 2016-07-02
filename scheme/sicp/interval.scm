;;;; 2.1.4 Extended Exercise: Interval Arithmetic
(import (rnrs base))
(define av assertion-violation)

(define (make-interval lower upper)
  (if (< upper lower)
      (av 'make-interval "lower/upper wrong way around" lower upper)
      (cons lower upper)))

(define (interval-from-list ls)
  (make-interval (first ls) (second ls)))

(define lower-bound car)
(define upper-bound cdr)
  
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

;; from text, p2 and p3 seem redundant?
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x))
     2.0))

;; Exercise 2.9
(define (show-width-sum)
  "show width of sum is a function of width of arguments"
  (let ((x (make-interval 1 2))
        (y (make-interval 3 4)))
    (unless (= (+ (width-interval x) (width-interval y))
               (width-interval (add-interval x y)))
      (av 'show-width-sum "proof failed" #f))))

(define (check-no-span-zero x)
  "error if x spans zero"
  (if (span-zero? x)
      (av 'check-no-span-zero "interval spans zero" x)))

(define (span-zero? x)
  (and (< (lower-bound x) 0) (> (upper-bound x) 0)))


;;;; Exercise 2.10
;; has a wee little bug for case bc and ac (need to flip them around)
(define (mul-optimised-interval x y)
  (cond ((type-a? x)
         (cond ((or (type-a? y) (type-b? y))
                (make-interval (* (lower-bound x) (upper-bound y))
                               (* (upper-bound x) (upper-bound y))))
               (else                    ; type-c y
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (lower-bound y))))))
        ((type-b? x)
         (cond ((type-a? y)
                (make-interval (* (lower-bound x) (upper-bound y))
                               (* (upper-bound x) (upper-bound y))))
               ((type-b? y)
                (make-interval (* (lower-bound x) (lower-bound y))
                               (* (upper-bound x) (upper-bound y))))
               (else                   ; type-c y
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (upper-bound y))))))
        (else                           ; type-c x
         (cond ((type-a? y)              
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (lower-bound y))))
               ((type-b? y)
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (upper-bound y))))
               ((type-c? y)
                (make-interval (* (upper-bound x) (upper-bound y))
                               (* (lower-bound x) (lower-bound y))))))))

(define (type-a? x)
  (span-zero? x))

(define (type-b? x)
  (and (positive? (lower-bound x)) (positive? (upper-bound x))))

(define (type-c? x)
  (and (negative? (lower-bound x)) (negative? (upper-bound x))))

(define (eq-interval x y)
  (and (= (lower-bound x) (lower-bound y))
       (= (upper-bound x) (upper-bound y))))



