;;; Queue implementaion (see page 54)

(define make-queue
  (lambda ()
    (let ([end (cons 'ignored '())])
      (cons end end))))

(define putq!
  (lambda (q v)
    (let ([end (cons 'ignored '())])
      (set-car! (cdr q) v)
      (set-cdr! (cdr q) end)
      (set-cdr! q end))))

(define getq
  (lambda (q)
    (car (car q))))

(define delq!
  (lambda (q)
    (if (= (lenq q) 0)
        (assertion-violation 'delq' "queue is empty" q)
        (set-car! q (cdr (car q))))))

(define lenq
  (lambda (q)
    (- (length (car q)) 1)))

(define emptyq?
  (lambda (q)
    (= (lenq q) 0)))
