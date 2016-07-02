;;;; Rectangle in a Plane ADT

(load "point.scm")

(define (make-rect a b c)
  "make a rectangle from 3 points"
  (list a b c (fourth-point a b c)))

(define (fourth-point a b c)
  "Calculate the fourth point of rectangle."
  (add-point b (sub-point c a)))
             
(define (print-rect r)
  (print-rect-point "a: " (a-rect r))
  (newline)
  (print-rect-point "b: " (b-rect r))
  (newline)
  (print-rect-point "c: " (c-rect r))
  (newline)
  (print-rect-point "d: " (d-rect r))
  (newline)
  (display-height r)
  (display-width r)
  (display-perimeter r)
  (display-area r))

(define (print-rect-point pre point)
  (display pre)
  (print-point point))

(define (a-rect r)
  (car r))

(define (b-rect r)
  (cadr r))

(define (c-rect r)
  (caddr r))

(define (d-rect r)
  (cadddr r))

(define (display-height r)
  (display "height: ")
  (display (height-rect r))
  (newline))

(define (display-width r)
  (display "width: ")
  (display (width-rect r))
  (newline))

(define (display-perimeter r)
  (display "perimeter: ")
  (display (perimeter-rect r))
  (newline))

(define (display-area r)
  (display "area: ")
  (display (area-rect r))
  (newline))

(define (height-rect r)
  (length-segment (make-segment (a-rect r) (b-rect r))))

(define (width-rect r)
  (length-segment (make-segment (a-rect r) (c-rect r))))

(define (area-rect r)
  (* (width-rect r)
     (height-rect r)))

(define (perimeter-rect r)
  (+ (* 2 (height-rect r))
     (* 2 (width-rect r))))
        
