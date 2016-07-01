(define make-counter
;;; spl page 50
  (lambda ()
    (let ([next 0])
      (lambda ()
        (let ([v next])
             (set! next (+ next 1))
             v)))))

(define make-counter2
  ;; initial value and increment amount
  (lambda (ival inc)
    (let ([next ival])
        (lambda ()
          (let ([v next])
            (set! next (+ next inc))
            v)))))
