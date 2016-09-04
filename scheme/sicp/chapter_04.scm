;;;; Various exercises from chapter 4
;;;;

(load-from-path "lib.scm")
;(load-from-path "interpreter.scm")
#!
;;;; Exercise 4.1

(define (list-of-values-lr exps env)
  "left to right evaluation"
  (if (no-operands? exps)
      '()
      (let ((res (eval (first-operand exps) env)))
        (cons res
              (list-of-values-lr (rest-operands exps) env)))))

(define (list-of-values-rl exps env)
  "right to left evaluation"
  (if (no-operands? exps)
      '()
      (let ((res (list-of-values-rl (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              res))))

;;;; Exercise 4.2

;; Procedure Application, form (call <op> <operand> ... <operand>)

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp)
  (cadr exp))

(define (operands exp)
  (cddr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

;;;; Exercise 4.3

(define (tag exp)
  (if (pair? exp)
      (car exp)
      'invalid))
  
(define operations-table (make-table))
(define get (operations-table 'lookup-proc))
(define put (operations-table 'insert!))

(put 'variable? lookup-variable-value)
(put 'quoted? text-of-quotation)
(put 'assignment? eval-assignment)
(put 'definition eval-definition)
(put 'if? eval-if)

(put 'lambda? wrap-lambda)
(put 'begin? wrap-begin)
(put 'cond? wrap-cond)
(put 'application? wrap-application)

(define (wrap-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(define (wrap-begin exp env)
  (eval-sequence (begin-actions exp) env))

(define (wrap-cond exp env)
  (eval (cond->if exp) env))

(define (wrap-application exp env)
  (apply (eval (operator exp) env)
         (list-of-values (operands exp) env)))

(define (eval exp env)
  (if (self-evaluating? exp)
      exp
      (let ((proc (get (tag exp))))
        (if proc
            (proc exp env)
            (error "Unknown procedure -- EVAL" (tag exp))))))

;;;; Exercise 4.4


;; and

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-clauses exp)
  (cdr exp))

(define (first-and-clause clauses)
  (car clauses))

(define (rest-and-clauses clauses)
  (cdr clauses))

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
   (cond ((null? clauses) true)
         ((last-and-clause? clauses)
          (eval (first-and-clause clauses)))
         (else
          (make-if (first-and-clause clauses)
                   (expand-and-clauses (rest-and-clauses clauses))
                   false))))

;; or

(define (or? exp)
  (tagged-list? exp 'or))

(define (or-clauses exp)
  (cdr exp))

(define (first-or-clause clauses)
  (car clauses))

(define (rest-or-clauses clauses)
  (cdr clauses))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
   (if (null? clauses)
       false
       (let ((result (eval (first-or-clause clauses))))
         (if result
             result
             (expand-or-clauses (rest-or-clauses clauses))))))
             
!#

;;; Exercise 4.11

;; Rewrite environment representation

(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

(define (empty-environment? env)
  (null? env))

(define (make-frame variables values)
  (cons 'frame
        (map (lambda (var val)
               (list var val))
             variables
             values)))

(define (bindings-frame frame)
  (cdr frame))

(define (make-binding var val)
  (cons var val))

(define (var-binding b)
  (car b))

(define (val-binding b)
  (cdr b))

(define (frame-variables frame)
  (map car (bindings-frame frame)))

(define (frame-values frame)
  (map cdr (bindings-frame frame)))

(define (bound-in-frame? var frame)
  (let iter ((bindings (bindings-frame frame)))
    (cond ((null? bindings) #f)
          ((eq? (var-binding (car bindings)) var) #t) 
          (else (iter (cdr bindings))))))

(define (binding-from-frame var frame)
  (let iter ((bindings (bindings-frame frame)))
    (cond ((null? bindings)
           (error "unbound variable -- BINDING-FROM-FRAME" var))
          ((eq? (var-binding (car bindings)) var)
           (val-binding (car bindings)))
          (else (iter (cdr bindings))))))

(define (set-binding-in-frame! var val frame)
  (let iter ((bindings (bindings-frame frame)))
    (cond ((null? bindings)
           (error "unbound variable -- SET-BINDING-IN-FRAME!" var))
          ((eq? (var-binding (car bindings)) var)
           (set-car! (car bindings) val))
          (else (iter (cdr bindings))))))

(define (add-binding-to-frame! var val frame)
  (if (bound-in-frame? var frame)
      (error "variable already bound -- ADD-BINDING-TO-FRAME!" var)
      (set-cdr! frame (cons (make-binding var val)
                            (bindings-frame frame)))))

;;;; Exercise 4.13
;; specify make-unbound!: removes binding from all environments

(define (make-unbound! var env)
  (let iter ((env env))
    (if (empty-environment? env)
        (error "unbound variable -- MAKE-UNBOUND!" var)
        (let ((frame (first-frame env)))
          (if (bound-in-frame? var frame)
              (unbind-from-frame var frame)
              (iter (enclosing-environment env)))))))

(define (unbind-from-frame var frame)
  (define (bindings-without-var-frame var frame)
    (let remove-var-bindings ((bindings (bindings-frame frame)))
      (cond ((null? bindings) '())
            ((eq? (var-binding (car bindings)) var) (cdr bindings))
            (else (cons (car bindings) (remove-var-bindings (cdr bindings)))))))
  (set-cdr! frame (bindings-without-var-frame var frame)))

;;;; Exercise 4.35

(define (an-integer-starting-from n)
  (amd n (an-integer-starting-from (inc n))))

(define (an-integer-between x y)
  (let ((n (an-integer-starting-from (inc x))))
    (require (< n y))
    n))

;;;; Exercise 4.36

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (all-pythagorean-triples)
  (let ((high (an-integer-starting-from 2)))
    (let ((i (an-integer-between 1 high)))
      (let ((j (an-integer-between i high)))
        (let let ((k (an-integer-between j high)))
          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k))))))
    
;;;; Exercise 4.40

(define (multiple-dwelling)
  (let ((fletcher (amb 2 3 4))
        (cooper (amb 2 3 4 5)))
    (require (not (= (abs (- fletcher cooper)))))
    (let ((smith (1 2 3 4 5)))
      (require (not (= (abs (- smith fletcher)))))
      (let ((miller (amb 1 2 3 4 5)))
        (require (> miller cooper))
        (let ((baker (amb 1 2 3 4)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

;;;; Exercise 4.43
;;
;; Fathers Mr Moore, Colonel Downing, Mr Hall, Sir Barnacle Hood, Dr Parker
;; Daughters Mary Anne, Gabrielle, Lorna, Rosalind, Melissa
;(define fathers '(mr-moore colnel-downing mr-hall sir-barnicle-hood dr-parker))

(define surnames '(moore downing hall hood parker))
(define daughters 'mary-anne gabrielle lona rosalind melissa)

(define (yahct)
  ;; daughters -> surnames
  (let ((mary-anne 'mr-moore)
        (gabrielle (amb surnames))
        (lona (amb surnames))
        (rosalind (amb surnames))
        (melissa (amb (surnames))))
    ;; fathers -> yachts (daughters names)
    (let ((moore (amb daughters))
          (downing (amb daughters))
          (hall (amb daughters))
          (hood (amb daughters))
          (parker (amb daughters)))
      (require (eq? hood 'gabrielle))
      (require (eq? moore 'lorna))
      (require (eq? hall 'rosalind))
      (require (eq? downing 'melissa))
      (require (eq? melissa 'hood))
      (require (eq? gabrielle ;; ???
                    ))))
  (list 
   (list (list 'mary-anne mary-anne)
         (list 'gabrielle gabrielle)
         (list 'lona lona)
         (list 'rosalind rosalind)
         (list 'melissa melissa))
   (list (list 'moore moore)
         (list 'downing downing)
         (list 'hall hall)
         (list 'hood hood)
         (list 'parker parker))))

;;; Exercise 4.44

