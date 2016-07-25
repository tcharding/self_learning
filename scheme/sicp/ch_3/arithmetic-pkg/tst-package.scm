;;;; tests for arithmetic-pkg.scm
(load-from-path "test-framework.scm")
(load-from-path "lib.scm")
(load-from-path "arithmetic-pkg/arithmetic.scm")


(set-test-section! "integers")

(define x (make-integer 1))
(define y (make-integer 2))

(test-eq "" (num-eq? x y) #f)
(test-eq "" (num-eq? x x) #t)

(test-eq "add" (num-eq? (add x y) (make-integer 3)) #t)

(test-eq "zero" (=zero? (make-integer 0)) #t)
(test-eq "zero" (=zero? (make-integer 1)) #f)
(test-eq "sub" (num-eq? (sub y x) 1) #t)
(test-eq "div" (num-eq? (div y x) (make-integer 2)) #t)
(test-eq "mul" (num-eq? (mul y x) (make-integer 2)) #t)

(define res (reduce (make-integer 2) (make-integer 4)))
(define exp (list (make-integer 1) (make-integer 2)))
(test-eq "reduce"
  (and (num-eq? (car res) (car exp))
       (num-eq? (cadr res) (cadr exp)))
  #t)

;;;;
(set-test-section! "ordinary numbers - untagged")

(define x (make-integer 1))
(define y 2)

(test-eq "" (num-eq? x y) #f)
(test-eq "" (num-eq? x x) #t)
(test-eq "sum" (num-eq? (add x y) (make-integer 3)) #t)
(test-eq "zero" (=zero? 0) #t)
(test-eq "zero" (=zero? 1) #f)

;;;;
(set-test-section! "rational")

(define half (make-rational 1 2))
(define quarter (make-rational 1 4))
(define three-quarters (make-rational 3 4))

(test-eq "" (num-eq? half quarter) #f)

(test-eq "add" (num-eq? three-quarters (add half quarter)) #t)
(test-eq "sub" (num-eq? (sub three-quarters half) quarter) #t)
(test-eq "div" (num-eq? (div half quarter) (make-rational 2 1)) #t)
(test-eq "mul" (num-eq? (mul half quarter) (make-rational 1 8)) #t)

(test-eq "zero" (=zero? half) #f)
(test-eq "zero" (=zero? (make-rational 0 2)) #t)
(test-eq "zero" (=zero? (sub half half)) #t)
(test-eq "zero" (=zero? (sub three-quarters half)) #f)

;;;;
(set-test-section! "real")

(define x (make-real 1))
(define y (make-real 2))

(test-eq "" (num-eq? x y) #f)
(test-eq "" (num-eq? x x) #t)

(test-eq "add" (num-eq? (add x y) (make-real 3)) #t)
(test-eq "zero" (=zero? (make-real 0)) #t)
(test-eq "zero" (=zero? (make-real 1)) #f)
(test-eq "sub" (num-eq? (sub y x) (make-real 1)) #t)
(test-eq "div" (num-eq? (div y x) (make-real 2)) #t)
(test-eq "mul" (num-eq? (mul y x) (make-real 2)) #t)

;;;;
(set-test-section! "complex")

(define z (make-from-real-imag 1 2))
(define w (make-from-real-imag 2 4))

(test-eq "" (real-part z) 1)
(test-eq "" (imag-part z) 2)

(test-eq "num-eq? #f" (num-eq? z w) #f)

(test-eq "num-eq? same #t" (num-eq? z z) #t)
(test-eq "add" (num-eq? w (add z z)) #t)
(test-eq "sub" (num-eq? (sub w z) z) #t)
(test-eq "div" (num-eq? (div w z) (make-from-mag-ang 2 0)) #t)
(test-eq "mul" (num-eq? (mul z w) (make-from-real-imag -6 8)) #t)

(test-eq "zero" (=zero? (sub z z)) #t)

;;;;
#!
(set-test-section! "raise")

(define x (make-integer 1))
(define rat (make-rational 1 1))
(define real (make-real 1))
(define z (make-from-real-imag 1 0))

(test-eq "" (num-eq? x rat) #t)
(test-eq "" (num-eq? x real) #t)
(test-eq "" (num-eq? x z) #t)
(test-eq "" (num-eq? rat real) #t)
(test-eq "" (num-eq? rat z) #t)
(test-eq "" (num-eq? real z) #t)
!#
;;;;
(set-test-section! "polynomial (basic)")

(define p1 (make-poly 'x '((2 3))))
(define p2 (make-poly 'x '((2 2))))
(define sum-p1-p2 (make-poly 'x '((2 5))))
(define p1-squared (make-poly 'x '((4 9))))
(define sub-p1-p2 (make-poly 'x '((2 1))))

(test-eq "zero" (=zero? p1) #f)
(test-eq "same" (num-eq? p1 p1) #t)  
(test-eq "add" (num-eq? sum-p1-p2 (add p1 p2)) #t)
(test-eq "sub same" (=zero? (sub p1 p1)) #t)
(test-eq "sub" (num-eq? sub-p1-p2 (sub p1 p2)) #t)
(test-eq "mul" (num-eq? p1-squared (mul p1 p1)) #t)

;;;;
(set-test-section! "polynomial (complex)")

(define p3 (make-poly 'x '((2 2) (1 1) (0 3)))) ; 2Xe2 + X + 3
(define p4 (make-poly 'x '((2 1) (0 4))))       ; Xe2 + 4
(define sum-p3-p4 (make-poly 'x '((2 3) (1 1) (0 7))))
(define p4-squared (make-poly 'x '((4 1) (2 8) (0 16))))
(define sub-p3-p4 (make-poly 'x '((2 1) (1 1) (0 -1))))

(test-eq "zero" (=zero? p3) #f)
(test-eq "same" (num-eq? p3 p3) #t)  
(test-eq "add" (num-eq? sum-p3-p4 (add p3 p4)) #t)
(test-eq "mul" (num-eq? p4-squared (mul p4 p4)) #t)
(test-eq "sub" (=zero? (sub p3 p3)) #t)
(test-eq "sub" (num-eq? sub-p3-p4 (sub p3 p4)) #t)

(define p1 (make-poly 'x '((5 1) (0 -1))))
(define p2 (make-poly 'x '((2 1) (0 -1))))
(define exp (list (make-poly 'x '((3 1) (1 1)))
                  (make-poly 'x '((1 1) (0 -1)))))
(define res (div p1 p2))

(test-eq "div"
  (and (num-eq? (car exp) (car res))
       (num-eq? (cadr exp) (cadr res)))
  #t)

(set-test-section! "complex (rational functions)")

(define p1 (make-poly 'x '((2 1) (0 1))))
(define p2 (make-poly 'x '((3 1) (0 1))))

(define rf (make-rational p2 p1))

(define 2rf (add rf rf))

(define p1 (make-poly 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-poly 'x '((3 1) (1 -1))))

(define simple-gcd (gcd p1 p2))

;; cause complication to gcd algorithm

(define p1 (make-poly 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-poly 'x '((2 11) (0 7))))
(define p3 (make-poly 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(define complicate-gcd (gcd q1 q2))

;; test make-rational reduces poly's

(define p1 (make-poly 'x '((2 1) (1 1))))
(define p2 (make-poly 'x '((2 1) (1 3))))

(define rf (make-rational p2 p1))

(define exp-p1 (make-poly 'x '((1 1) (0 1))))
(define exp-p2 (make-poly 'x '((1 1) (0 3))))
(define exp-rf (make-rational exp-p2 exp-p1))

(test-eq "make-rational reduced" (and (num-eq? (numer rf) (numer exp-rf))
                                      (num-eq? (denom rf) (denom exp-rf)))
         #t)

;; final test, Exercise 2.97 :)!!

(define p1 (make-poly 'x '((1 1) (0 1))))
(define p2 (make-poly 'x '((3 1) (0 -1))))
(define p3 (make-poly 'x '((1 1))))
(define p4 (make-poly 'x '((2 1) (0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

;(add rf1 rf2)

'ok

