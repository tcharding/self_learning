;;; Exercise 2.9.6

(define make-queue
  (lambda ()
    (let ([end (cons 'ignored '())])
      (cons end end))))

(define putq!
  (lambda (q v)
    (let ([end (cons 'ignored '())])
      (if (= 0 (lenq q))
          ; copy v into initial pair
          (set-car! q v)
          ; create new pair and copy v
          ((set-cdr! (cdr q) end)
           (set-car! (cdr q) v)
            (set-cdr! q end))))))

(define getq
  (lambda (q)
    (if (= (lenq q) 0)
        (assertion-violation 'getq "queue is empty" q)
        (car (car q))))

(define delq!
  (lambda (q)
    (if (= (lenq q) 0)
        (assertion-violation 'delq' "queue is empty" q)
        (set-car! q (cdr (car q))))))

(define lenq
  (lambda (q)
    (let ((len (length (car q))))
      (if (= 1 len)
          0
          (- len 1)))))

(define emptyq?
  (lambda (q)
    (= (lenq q) 0)))
