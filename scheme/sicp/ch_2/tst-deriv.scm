;;;; tests for deriv.scm
(load "../test-framework.scm")
(load "deriv.scm")
(load "../lib.scm")

;;;; test predicates

(test-eq "number? #t" (number? 2) #t)
(test-eq "number? #t" (number? 0) #t)

(test-eq "number? #f" (number? 'a) #f)
(test-eq "number? #f" (number? '()) #f)
(test-eq "number? #f" (number? '(2)) #f)

(test-eq "variable? #t" (variable? 'x) #t)
(test-eq "variable? #f" (variable? 2) #f)

(test-eq "same-variable? #t" (same-variable? 'x 'x) #t)
(test-eq "same-variable? #f" (same-variable? 'x 'y) #f)

(test-eq "sum? #t" (sum? '(+ x y)) #t)
(test-eq "sum? #t" (sum? '(+ 2 3)) #t)

(test-eq "sum? #f" (sum? '(* x y)) #f)
(test-eq "sum? #f" (sum? '(* 2 3)) #f)

(test-eq "product? #f" (product? '(+ x y)) #f)
(test-eq "product? #f" (product? '(+ 2 3)) #f)

(test-eq "product? #t" (product? '(* x y)) #t)
(test-eq "product? #t" (product? '(* 2 3)) #t)

(test-eq "exponential? #t" (exponential? '(expt 2 3)) #t)
(test-eq "exponential? #t" (exponential? '(expt x y)) #t)

(test-eq "exponential? #f" (exponential? '()) #f)

;;;; test Constructors and Selectors

(define x+y (make-sum 'x 'y))
(define x*y (make-product 'x 'y))

(test-eq "addend" (addend x+y) 'x)
(test-eq "augend" (augend x+y) 'y)

(test-eq "multiplier" (multiplier x*y) 'x)
(test-eq "multiplicand" (multiplicand x*y) 'y)

                          
