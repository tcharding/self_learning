; The Little Schemer
;  Chapter 10 - What Is the Value of All of This?

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 '()))))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name s1 s2 not-found-f)
    (cond
     ((null? s1) not-found-f)
     ((equal? (car s1) name) (car s2))
     (else (lookup-in-entry-help name (cdr s1) (cdr s2) not-found-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name
                       (car table)
                       (lambda (name)
                         (lookup-in-table name (cdr table) table-f)))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? '+ (car nexp))
      (plus (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
     ((eq? 'x (car nexp))
      (times (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
     (else
      (exp (value (car (cdr nexp))) (value (car (cdr (cdr nexp)))))))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (a)
    (cond
     ((number? a) *const)
     ((equal? a #t) *const)
     ((equal? a #f) *const)
     ((qual? a 'cons) *const)
     ((qual? a 'car) *const)
     ((qual? a 'cdr) *const)
     ((qual? a 'null?) *const)
     ((qual? a 'eq?) *const)
     ((qual? a 'atom?) *const)
     ((qual? a 'zero?) *const)
     ((qual? a 'add1) *const)
     ((qual? a 'sub1) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? #t) #t)
     ((eq? #f) #f)
     (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive)
    (cons table (cdr e))))
    
(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define question-of first)

(define answer-of second)

(define else?
  (lambda (x)
    (cond
     (atom? x) (eq? x 'else)
     (else #f))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (l table)
    (cond
     ((null? l) '())
     (cons (meaning (car l) table) (evlis (cdr l) table)))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun) (apply-primitive (second fun) vals))
     ((non-primitive fun) (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) 'primitive) #t)
     ((eq? (car x) 'non-primitive) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
                           (tabel-of closure)))))
              
