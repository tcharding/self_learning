;;;; Binary Mobile
;;;;
;;;; Exercise 2.29
(load "../lib.scm")

;;; Constructors

(define (make-mobile left right)
  (list left right))

(define (make-branch length payload)
  (list length payload))

;;; Selectors

(define (left-branch m)
  (car m))
  
(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))
  
(define (branch-payload b)
  (cadr b))

;;; Equality

(define (branch-eq? b c)
  (or (and (and (holds-weight? b) (holds-weight? c))
           (= (branch-payload b) (branch-payload c)))
      (and (and (holds-mobile? b) (holds-mobile? c))
           (mobile-eq? (branch-payload b) (branch-payload c)))))

(define (mobile-eq? m n)
  "mirror images are not deemed equal"
  (and (branch-eq? (left-branch m) (left-branch n))
       (branch-eq? (right-branch m) (right-branch n))))

;;; Output

(define (print-mobile m)
  (print-mobile-hlpr m 1))


(define (print-mobile-hlpr m level)
  (print-branch (left-branch m) level)
  (print-branch (right-branch m) level))
                
(define (print-branch b level)
  (if (holds-mobile? b)
      (print-mobile-hlpr b (inc level))
      (print-weight (branch-payload b) level)))

(define (print-weight w level)
  (display-spaces level)
  (d "-")
  (dnl w))

;;; Weight 

(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define (branch-weight b)
    (if (holds-weight? b)
        (branch-payload b)
        (total-weight (branch-payload b))))

(define (mobile-balanced? m)
  (and (branch-balanced? (left-branch m))
       (branch-balanced? (right-branch m))
       (= (torque (left-branch m))
          (torque (right-branch m)))))

(define (branch-balanced? b)
  (if (holds-weight? b)
      #t
      (mobile-balanced? (branch-payload b))))

(define (torque b)
  (* (branch-length b) (branch-weight b)))

(define (holds-weight? b)
  (weight? (branch-payload b)))

(define (holds-mobile? b)
  (not (holds-weight? b)))

(define (weight? payload)
  (number? payload))
