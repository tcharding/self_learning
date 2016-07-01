;; page 44

(define (cube x)
  (* x x x))


(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define counter 0)
;; (define sine
;;   (lambda (angle)
;;     (cond
;;      ((< (abs angle) 0.1)
;;       (out "counter: " counter " angle: " angle 'nl))
;;       (else (set! counter (+ 1 counter))
;;             (p (sine (/ angle 3.0)))))))
          

(define out
  (lambda ls
    (for-each display-obj ls)))

(define display-obj
  (lambda (obj)
    (cond
     ((eqv? obj 'nl) (newline))
     (else (display obj)))))
  
(define sine
  (lambda (radians)
    (if (not (> (abs radians) 0.1))
         angle
     (p (sine (/ radians 3.0))))))
      
  
