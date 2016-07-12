;;;; Section 2.3.2 Symbolic Differentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponential? exp)
         (make-product
          (make-product (exponent exp)
                        (expt (base exp) (dec (exponent exp))))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;;;; Constructors

(define (make-sum . sum)
  (let iter ((old-sum sum) (new-sum '()) (acc 0))
    (cond ((null? old-sum)
           (sum-from-vars-and-const (reverse new-sum) acc))
          ((sum? old-sum)
           (iter (cdr old-sum) new-sum acc)) ; skip '+
          ((number? (car old-sum))
           (iter (cdr old-sum) new-sum (+ acc (car old-sum))))
          (else (iter (cdr old-sum) (cons (car old-sum) new-sum) acc)))))

(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
         (else (list '* x y))))

(define (sum-from-vars-and-const ls num)
  (cond ((zero? num)
         (cons '+ ls))
        (else (cons '+ (append ls (list num))))))
          
;;;; Selectors

(define (addend e)
  (if (= (length e) 1)
      0                                 ; (+)
      (cadr e)))

(define (augend e)
  (cond ((= (length e) 1) 0)            ; (+)
        ((= (length e) 2) 0)            ; (+ 2)
        ((= (length e) 3) (caddr e))      ; (+ 2 3)
        (else (apply make-sum (cddr e))))) ; (+ 2 3 4 ...)

(define (multiplier e)
  (cadr e))

(define (multiplicand e)
  (caddr e))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

;;;; Predicates

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? e)
  (and (pair? e)
       (eq? (car e) '+)))

(define (product? e)
  (and (pair? e)
       (eq? (car e) '*)))
         
(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (exponential? e)
  (and (pair? e)
       (eq? (car e) 'expt)))
