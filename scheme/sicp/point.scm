;;;; Point ADT (x, y)

(define (make-point x y)
  (cons x y))

(define x-point car)
(define y-point cdr)
  
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (add-point p q)
  (make-point (+ (x-point p) (x-point q))
              (+ (y-point p) (y-point q))))

(define (sub-point p q)
  (make-point (- (x-point p) (x-point q))
              (- (y-point p) (y-point q))))

(define (halve-point p)
  (make-point (/ (x-point p) 2)
              (/ (y-point p) 2)))

  
