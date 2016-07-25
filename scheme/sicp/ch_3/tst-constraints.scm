;;;; tests for constraints.scm
(load-from-path "test-framework.scm")
(load-from-path "constraints.scm")
(load-from-path "lib.scm")

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(define my-adder (adder a b c))
(define my-multiplier (multiplier a b c))

;;; temp converter

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-corverter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)

;;; test average constraint 

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe "A" a)
(probe "B" b)
(probe "C" c)

(set-value! a 20 'user)
(set-value! b 10 'user)
(average a b c)
 
;;; test squarer

(define a (make-connector))
(define b (make-connector))

(forget-value! a 'user)
(forget-value! b 'user)

(probe "A" a)
(probe "B" b)

(set-value! a 2 'user)
(squarer a b)

(forget-value! a 'user)
(set-value! b 9 'user)

