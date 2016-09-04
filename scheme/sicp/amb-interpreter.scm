;;;; AMB Evaluator
;;;;
;;;; SICP, Section 4.3.3

;; Run this in guile before loading
;(define apply-in-underlying-scheme apply)

(define false #f)
(define true #t)

;;; Syntax

;;

(define (amb? exp)
  (tagged-list? exp 'amb))

(define (amb-choices exp)
  (cdr exp))

;; The only self evaluating forms are numbers and strings

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) false)
        (else false)))

;; Variables are represented by symbols

(define (variable? exp)
  (symbol? exp))

;; Quotations have the form (quote <text-of-quotation>)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

;; Tags

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Assignments have form (set! <var> <value>)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

;; Definitions have form (define <var> <value>) or
;; (define (<var> <parameter> ... <parameter>)
;;   (<body>))

(define (make-definition name value)
  (list 'define name value))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)          ; formal parameters
                   (cddr exp))))        ; body

;; Lambda

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; Conditionals

(define (if? exp)
  (tagged-list? exp 'if))
  
(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (let ((alternative (cadddr exp)))
    (if (not (null? alternative))
        alternative
        'false)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
  
;; Sequences

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

;; Procedure Application
;;  any compound expression that is not one of the above

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

;;; Derived expressions

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

;;;; Exercise 4.5
#!
;; original SICP version
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
!#

(define (eval-cond exp env)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (cond ((cond-else-clause? first)
                 (if (null? rest)
                     (sequence->exp (cond-actions first))
                     (error "ELSE clause isn't last -- COND->IF" clauses)))
                ((cond-arrow-clause? first)
                 (let ((result (eval (cond-predicate first) env)))
                   (make-if result
                            ((cond-actions first) result)
                            'false)))
                (else 
                 (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest)))))))
  (eval (expand-clauses (cond-clauses exp)) env))


(define (cond-arrow-clause? clause)
  (eq? (cadr clause) '=>))

(define (cond-actions clause)
  (if (cond-arrow-clause? clause)
      (cddr clause)
      (cdr clause)))


;;;

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
;        ((or? exp) (analyze-or exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begin-actions exp)))
;        ((cond? exp) (analyze (cond->if exp)))
;        ((and? exp) (eval-and exp env))
;        ((let? exp) (eval (let->combination exp) env))
;        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((application? exp) (analyze-application exp))
        ((amb? exp) (analyze-amb exp))
        ((if-fail? exp) (analyze-if-fail exp))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda env)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
            ;; success continuation for evaluating predicate
            ;; to obtain pred-value
            (lambda (pred-value fail2)
              (if (true? pred-value)
                         (cproc env succeed fail2)
                         (aproc env succeed fail2)))
              ;; failure continuation for evaluating predicate
            fail))))

(define (analyze-if-fail exp)
  (let ((pproc (analyze (if-fail-predicate exp)))
        (fail-result (if-fail-consequent exp)))
    (lambda (env succeed fail)
      (let ((result (pproc env)))
        ;; success continuation for evaluating predicate
        ;; to obtain pred-value
        (lambda (pred-value fail2)
          (if (true? pred-value)
              (cproc env succeed fail2)
              (aproc env succeed fail2)))
              ;; failure continuation for evaluating predicate
        fail-result))))


(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))

  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))

  (let ((pprocs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE-SEQUENCE" exps))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)))))
             (fail)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   ;; success continuation for recursive
                   ;; call to get-args
                   (lambda (args fail3)
                     (succeed (cons arg args)
                              fail3))
                   fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else (error "Unknown procedure -- EXECUTE-APPLICATION" proc))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  "return the value of the last expression"
  (cond ((last-exp? exps)
         (eval (first-exp exps) env)) 
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;; Evaluator data structures

;;

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; Representing procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))

;; Operations on environments

;; Environment representation

(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

(define (empty-environment? env)
  (null? env))

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (bound-in-frame? var frame)
  (let iter ((vars (frame-variables frame))
             (vals (frame-values frame)))
    (cond ((null? vars) false)
          ((eq? (car vars) var) true)
          (else (iter (cdr vars) (cdr vals))))))

(define (binding-from-frame var frame)
  (let iter ((vars (frame-variables frame))
             (vals (frame-values frame)))
    (cond ((null? vars)
           (error "unbound variable -- BINDING-FROM-FRAME" var))
          ((eq? (car vars) var) (car vals))
          (else (iter (cdr vars) (cdr vals))))))

(define (set-binding-in-frame! var val frame)
  (let iter ((vars (frame-variables frame))
             (vals (frame-values frame)))
    (cond ((null? vars)
           (error "unbound variable -- SET-VARIABLE-VALUE-FRAME!" var))
          ((eq? (car vars) var)
           (set! (car vals) val))
          (else (iter (cdr vars) (cdr vals))))))

(define (add-binding-to-frame! var val frame)
  (if (bound-in-frame? var frame)
      (error "variable already bound -- ADD-BINDING-TO-FRAME!" var)
      (begin
        (set-car! frame (cons var (car frame)))
        (set-cdr! frame (cons val (cdr frame))))))

;; Environment procedures

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (set-variable-value! var val env)
  (act-on-bound-variable
   set-binding-in-frame!
   "Unbound variable -- SET-VARIABLE-VALUE"
   var
   val
   env))
   
(define (lookup-variable-value var env)
  (define (get-value)
    (act-on-bound-variable
     wrap-binding-from-frame
     "Unbound variable -- LOOKUP-VARIABLE-VALUE"
     var
     'ignored-val
     env))
  ; Exercise 4.16 part a (did not complete b or c)
  (let ((value (get-value)))
    (if (eq? value '*unassigned*)
        (error "Unassigned variable -- LOOKUP-VARIABLE-VALUE" var)
        value)))

(define (wrap-binding-from-frame var ignored-val frame)
  "three parameter version (needed by act-on-bound-variable)"
  (binding-from-frame var frame))

(define (act-on-bound-variable proc error-message var val env)
  (let iter ((env env))
    (if (empty-environment? env)
        (error error-message var)
        (let ((frame (first-frame env)))
          (if (bound-in-frame? var frame)
              (proc var val frame)
              (iter (enclosing-environment env)))))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (bound-in-frame? var frame)
        (set-binding-in-frame! var val frame)
        (add-binding-to-frame! var val frame))))

;;; 4.1.4 Running the evaluator as a program

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '/ /)
        (list '+ +)
        (list '- -)
        (list '* *)
        ; ...
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
              (if (null? choices)
                  (fail)
                  ((car choices)
                   env
                   succeed
                   (lambda ()
                     (try-next (cdr choices))))))
      (try-next cprocs))))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (display ";;; Starting a new problem ")
          (ambeval input
                   the-global-environment
                   ;; ambeval success
                   (lambda (val next-alternative)
                     (announce-output output-prompt)
                     (user-print val)
                     (internal-loop next-alternative))
                   ;; ambeval failure
                   (lambda ()
                     (announce-output
                      ";;; There are no more values of")
                     (user-print input)
                     (driver-loop))))))

  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

#!
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
!#

(define (prompt-for-input string)
  (newline) (newline) (dnl string))

(define (announce-output string)
  (newline) (dnl string))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
       
(define the-global-environment (setup-environment))
