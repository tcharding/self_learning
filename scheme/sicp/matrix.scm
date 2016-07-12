;;;; Matrices
(load "signal.scm")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (combine-mul row v))
       m))

(define (combine-vectors op v w)
  "v and w are same length vectors"
  (if (null? v)
      '()
      (cons (op (car v) (car w))
            (combine-mul (cdr v) (cdr w)))))
  
(define (combine-mul v w)
  "return vector t where, ti = vi . wi"
  (combine-vectors * v w))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map combine-mul m cols)))
   
