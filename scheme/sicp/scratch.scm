;; miscellaneous procedures


;; Read time conditional's
(define foo
  (lambda (x)
    (cond
     ((zero? x) 0)
     ((= 1 x) 1)
     #;(else x))))

;; Example indentation when operator is a list
(define bar
; use first form if line must be slit
  (lambda (ls)
      ((car ls)
       (cdr ls)
       foo)))
;; default emacs indentation
;      ((car ls) (cdr ls)
;       foo)))
;
;; this would be nice also though (default emacs does not do this)
;      ((car ls) (cdr ls)
;                foo)))
       
