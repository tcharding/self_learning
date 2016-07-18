;;;; tests for arithmetic-pkg.scm
(load-from-path "test-framework.scm")
(load-from-path "lib.scm")
(load-from-path "arithmetic-pkg/arithmetic.scm")


(set-test-section! "integers")
(define x (make-integer 1))
(define y (make-integer 2))

(test-eq "" (num-eq? x y) #f)
;(test-eq "" (num-eq? x x) #t)


(test-eq "add" (num-eq? (add x y) (make-integer 3)) #t)
(test-eq "zero" (=zero? (make-integer 0)) #t)
(test-eq "zero" (=zero? (make-integer 1)) #f)
(test-eq "sub" (num-eq? (sub y x) 1) #t)
(test-eq "div" (num-eq? (div y x) (make-integer 2)) #t)
(test-eq "mul" (num-eq? (mul y x) (make-integer 2)) #t)


(set-test-section! "ordinary numbers - untagged")

(define x (make-integer 1))
(define y 2)

(test-eq "" (num-eq? x y) #f)
(test-eq "" (num-eq? x x) #t)
(test-eq "sum" (num-eq? (add x y) (make-integer 3)) #t)
(test-eq "zero" (=zero? 0) #t)
(test-eq "zero" (=zero? 1) #f)

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


(set-test-section! "complex")

(define c (make-from-real-imag 1 2))
(define d (make-from-real-imag 2 4))

(test-eq "" (real-part c) 1)
(test-eq "" (imag-part c) 2)

(test-eq "num-eq? #f" (num-eq? c d) #f)

(test-eq "num-eq? same #t" (num-eq? c c) #t)
(test-eq "add" (num-eq? d (add c c)) #t)
(test-eq "sub" (num-eq? (sub d c) c) #t)
(test-eq "div" (num-eq? (div d c) (make-from-mag-ang 2 0)) #t)
(test-eq "mul" (num-eq? (mul c d) (make-from-real-imag -6 8)) #t)

(test-eq "zero" (=zero? (sub c c)) #t)

(set-test-section! "raise")

(define x (make-integer 1))
(define rat (make-rational 1 1))
(define real (make-real 1))
(define z (make-from-real-imag 1 0))

(test-eq "" (num-eq? x rat) #t)
(test-eq "" (num-eq? x real) #t)
;(test-eq "" (num-eq? x z) #t)
;(test-eq "" (num-eq? r z) #t)
