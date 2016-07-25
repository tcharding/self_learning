;;;; Symbolic Algebra
;;;;
;;;; SICP, section 2.5.3

(define (polynomial? x)
  (eq? (type-tag x) 'polynomial))

(define (install-polynomial-package)
  ;;; internal procedures

  ;; representation of polynomial
  
  (define (make-poly variable termlist)
    (cons variable termlist))

  (define (variable p)
    (car p))
  
  (define (term-list p)
    (cdr p))
  
  ;; representation of terms

  (define (make-term order coeff)
    (list order coeff))
  
  (define (order term)
    (car term))
    
  (define (coeff term)
    (cadr term))

  ;; representation of term lists

  (define (make-empty-termlist)
    '())

  (define (first-term termlist)
    (car termlist))
  
  (define (rest-terms termlist)
    (cdr termlist))

  (define (adjoin-term term termlist)
    (if (=zero? (coeff term))
        termlist
        (cons term termlist)))   

  (define (empty-termlist? termlist)
    (null? termlist))

  ;; polynomial procedures
  
  (define (eq-poly? p1 p2)
    (and (same-variable? (variable p1) (variable p2))
         (eq-termlist? (term-list p1) (term-list p2))))

  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  (define (variable? x)
    (symbol? x))

  (define (add-poly p1 p2)
    (handoff-to-termlist add-termlist "ADD" p1 p2))

  (define (sub-poly p1 p2)
    (handoff-to-termlist sub-termlist "SUB" p1 p2))

  (define (mul-poly p1 p2)
    (handoff-to-termlist mul-termlist "MUL" p1 p2))

  (define (gcd-poly p1 p2)
    (handoff-to-termlist gcd-termlist "GCD" p1 p2))

  (define (handoff-to-termlist fn message p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (fn (term-list p1)
                       (term-list p2)))
        (error (string-append "Polys not in same var -- " message "-POLY")
               (list p1 p2))))

  (define (div-poly p1 p2)
    (handoff-to-termlist-receive-pair div-termlist "DIV" p1 p2))

  (define (reduce-poly p1 p2)
;    (debug "reduce-poly" p1 p2)
    (handoff-to-termlist-receive-pair reduce-termlist "REDUCE" p1 p2))

  (define (handoff-to-termlist-receive-pair fn message p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((t (variable p1))
              (result (fn (term-list p1)
                          (term-list p2))))
          (list (make-poly t (car result))
                (make-poly t (cadr result))))
        (error (string-append "Polys not in same var -- " message "-POLY")
               (list p1 p2))))

  ;; term list procedures

  (define (eq-termlist? tl-1 tl-2)
    (cond ((and (empty-termlist? tl-1)
                (empty-termlist? tl-2))
           #t)
          ((empty-termlist? tl-1) #f)
          ((empty-termlist? tl-2) #f)
          (else 
           (and (eq-term? (first-term tl-1)
                          (first-term tl-2))
                (eq-termlist? (rest-terms tl-1)
                              (rest-terms tl-2))))))
  
  (define (add-termlist tl-1 tl-2)
    (cond ((empty-termlist? tl-1) tl-2)
          ((empty-termlist? tl-2) tl-1)
          (else
           (let ((t-1 (first-term tl-1)) (t-2 (first-term tl-2)))
             (cond ((gt (order t-1) (order t-2))
                    (adjoin-term t-1 (add-termlist (rest-terms tl-1) tl-2)))
                   ((lt (order t-1) (order t-2))
                    (adjoin-term t-2 (add-termlist tl-1 (rest-terms tl-2))))
                   (else
                    (adjoin-term (make-term (order t-1)
                                            (add (coeff t-1)
                                                 (coeff t-2)))
                                 (add-termlist (rest-terms tl-1)
                                               (rest-terms tl-2)))))))))

  (define (sub-termlist tl-1 tl-2)
    (cond ((empty-termlist? tl-1) (negate-termlist tl-2))
          ((empty-termlist? tl-2) tl-1)
          (else
           (let ((t-1 (first-term tl-1)) (t-2 (first-term tl-2)))
             (cond ((gt (order t-1) (order t-2))
                    (adjoin-term t-1 (sub-termlist (rest-terms tl-1) tl-2)))
                   ((lt (order t-1) (order t-2))
                    (adjoin-term (negate-term t-2) (sub-termlist tl-1 (rest-terms tl-2))))
                   (else
                    (adjoin-term (make-term (order t-1)
                                            (sub (coeff t-1) (coeff t-2)))
                                 (sub-termlist (rest-terms tl-1)
                                               (rest-terms tl-2)))))))))

  (define (negate-termlist tl)
    (if (empty-termlist? tl)
        tl
        (cons (negate (car tl))
              (negate-termlist (cdr tl)))))

  (define (mul-termlist tl-1 tl-2)
    (if (empty-termlist? tl-1)
        tl-1
        (add-termlist (mul-term-by-all-terms (first-term tl-1) tl-2)
                      (mul-termlist (rest-terms tl-1) tl-2))))

  (define (mul-term-by-all-terms t-1 ls)
    (if (empty-termlist? ls)
        ls
        (let ((t-2 (first-term ls)))
          (adjoin-term (make-term (add (order t-1) (order t-2))
                                  (mul (coeff t-1) (coeff t-2)))
                       (mul-term-by-all-terms t-1 (rest-terms ls))))))

  (define (div-termlist tl-1 tl-2)
      "returns pair (quotient remainder)"
      (if (empty-termlist? tl-1)
          (let ((list-res (list (make-empty-termlist) (make-empty-termlist))))
;            (debug "list-res" list-res)
            list-res)
          (let ((t-1 (first-term tl-1))
                (t-2 (first-term tl-2)))
            (if (gt (order t-2) (order t-1))
                (list (make-empty-termlist) tl-1)
                (let ((new-c (div (coeff t-1) (coeff t-2)))
                      (new-o (sub (order t-1) (order t-2))))
                  (let ((rest-of-result
                         (div-termlist
                          (sub-termlist
                           tl-1
                           (mul-termlist
                            (termlist-from-term (make-term new-o new-c))
                            tl-2))
                          tl-2)))
                    (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                          (cadr rest-of-result))))))))

  (define (termlist-from-term t)
    (adjoin-term t (make-empty-termlist)))

  (define (gcd-termlist tl-1 tl-2)
      (if (empty-termlist? tl-2)
          tl-1
          (gcd-termlist tl-2 (pseudo-remainder-termlist tl-1 tl-2))))

  (define (remainder-termlist tl-n tl-d)
    (cadr (div-termlist tl-n tl-d)))

  (define (pseudo-remainder-termlist tl-n tl-d)
    (define (integerizing-factor tl-n tl-d)
      (let ((co (coeff (first-term tl-d)))
            (order-n (order (first-term tl-n)))
            (order-d (order (first-term tl-d))))
        (num-expt co
                  (add 1
                       (add order-n
                            order-d)))))
    (cadr (div-termlist (mul-termlist-by-constant tl-n (integerizing-factor tl-n tl-d))
                        tl-d)))

  (define (mul-termlist-by-constant tl c)
    (foreach-termlist (lambda (t)
                        (make-term (order t)
                                   (mul c (coeff t))))
                      tl))

  (define (div-termlist-by-constant tl constant)
;    (debug "div-t-by-c" tl constant)
    (foreach-termlist (lambda (t)
                        (make-term (order t)
                                   (div (coeff t) constant)))
                      tl))
  
  (define (foreach-termlist fn tl)
    "make termlist by calling fn on each term of tl"
    (if (empty-termlist? tl)
        tl
        (adjoin-term (fn (first-term tl))
                     (foreach-termlist fn (rest-terms tl)))))


  (define (reduce-termlist n d)
    "returns (nn dd), n and d reduced to lowest terms"
    (define (integerizing-factor g n d)
      (let ((co (coeff (first-term g)))
            (order-1 (maximum (order (first-term n))
                              (order (first-term d))))
            (order-2 (order (first-term g))))
;        (debug "i-f" co order-1 order-2)
        (num-expt co (add 1 (sub order-1 order-2)))))

    (let ((g (gcd-termlist n d)))
;      (debug "g" g)
      (let ((int-factor (integerizing-factor g n d)))
;        (debug "int-factor" int-factor)
        (let ((n*i-f (mul-termlist-by-constant n int-factor))
              (d*i-f (mul-termlist-by-constant d int-factor)))
;          (debug "n and d" n*i-f d*i-f)
          (let ((n*i-f/g (div-termlist n*i-f g))
                (d*i-f/g (div-termlist d*i-f g)))
            
              ;; WHY THE HELL DO WE NEED CAR IN THESE TWO CALLS! (bug in div-termlist?)
            (let ((n*i-f/g (car n*i-f/g))
                  (d*i-f/g (car d*i-f/g)))
            (let ((coeff-gcd (coeff-gcd-termlist n*i-f/g d*i-f/g)))
              (list (div-termlist-by-constant n*i-f/g coeff-gcd)
                    (div-termlist-by-constant d*i-f/g coeff-gcd)))))))))

  (define (coeff-gcd-termlist tl-1 tl-2)
    "returns an integer gcd"
    (define (gcd-list ls)
      (cond ((null? ls) #f)
            ((null? (cdr ls)) (car ls))
            (else (gcd (car ls) (gcd-list (cdr ls))))))
    (gcd-list (append (map coeff tl-1)
                      (map coeff tl-2))))


  ;; term procedures

  (define (eq-term? t-1 t-2)
    (and (num-eq? (order t-1) (order t-2))
         (num-eq? (coeff t-1) (coeff t-2))))

  (define (negate-term t)
    (make-term (order t) (sub 0 (coeff t))))

  ;;; interface to rest of the system
  
  (define (tag p)
    (attach-tag 'polynomial p))

  (put 'add
       '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))

  (put 'mul
       '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))

  (put 'div
       '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (list (tag (car result))
                 (tag (cadr result))))))

  (put 'sub
       '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (sub-poly p1 p2))))       

  (put 'greatest-common-divisor
       '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (gcd-poly p1 p2))))

  (put '=zero?
       '(polynomial)
       (lambda (p)
         (empty-termlist? (term-list p))))

  (put 'reduce
       '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (reduce-poly p1 p2)))
           (list (tag (car result))
                 (tag (cadr result))))))

  (put 'num-eq? '(polynomial polynomial) eq-poly?)

  (put 'make
       'polynomial
       (lambda (var termlist)
         (tag (make-poly var termlist))))
  
  'done)

