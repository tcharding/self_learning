;;;; Section 3.1.2

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (let ((maximum 100))
    (= (gcd (random maximum) (random maximum)) 1)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (dec trials-remaining) (inc trials-passed)))
          (else
           (iter (dec trials-remaining) trials-passed))))
  (iter trials 0))

;;;; Exercise 3.5

(define (estimate-pi-using-integral)
  (* 1.0 (estimate-integral in-unit-circle? (rect 0 2 0 2) 100)))

(define (estimate-integral predicate rectangle trials)
  (define (point-test)
    (predicate (make-random-point-in-rect rectangle)))
  (* (area-rect rectangle)
     (monte-carlo trials point-test)))

(define (in-unit-circle? p)
  "point p lies within unit circle centre (1, 1)?"
  (<= (+ (square (- (x-point p) 1.0))
         (square (- (y-point p) 1.0)))
      1.0))

(define (make-random-point-in-rect r)
  (make-point (random-in-range (x-lower r) (x-upper r))
              (random-in-range (y-lower r) (y-upper r))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;; Rectangle

(define (rect x-lower x-upper y-lower y-upper)
  (list x-lower x-upper y-lower y-upper))

(define (area-rect r)
  (* (- (x-upper r) (x-lower r))
     (- (y-upper r) (y-lower r))))

(define x-lower car)
(define x-upper cadr)
(define y-lower caddr)
(define y-upper cadddr)

;;; Point

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

;;;
         
