;;;; Line Segment ADT
;;;;
;;;;  line segment is represented by two points (see point.scm)

(load "ch_1/sqrt.scm")

(define (make-segment p q)
  (cons p q))

(define (print-segment seg)
  (display "[")
  (print-point (start-segment seg))
  (display " - ")
  (print-point (end-segment seg))
  (display "]"))

(define start-segment car)
(define end-segment cdr)
  
(define (midpoint-segment seg)
  (add-point (start-segment seg)
             (halve-point (sub-point (end-segment seg)
                                     (start-segment seg)))))

(define (length-segment seg)
  (sqrt (+ (square (x-component seg))
           (square (y-component seg)))))

(define (x-component seg)
  (x-point (sub-point (end-segment seg) (start-segment seg))))

(define (y-component seg)
  (y-point (sub-point (end-segment seg) (start-segment seg))))
                      
                    
