(load "/home/tobin/build/scheme/little-schemer/libscm.scm")

(define t-equal
  (lambda (expected got)
    (cond
     ((eqan? expected got) #t)
     (else (cons "Fail: (expected/got):"
                 (cons expected
                       (cons got ())))))))

(define test-t-equal
  (lambda ()
    (t-equal 'a 'a)))
;    (t-equal 'a 'b)))

(define test-leftmost
  (lambda ()
    (and
     (eq? (leftmost '()) '())
     (eq? (leftmost '(a b c)) 'a)
     (eq? (leftmost '((a b) c d)) 'a))))

(define test-eqlist
  (lambda ()
    (and
     (t-equal #t (eqlist '(a) '(a)))
     (t-equal #t (eqlist '() '())))))
