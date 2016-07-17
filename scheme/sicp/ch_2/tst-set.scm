;;;; tests for set.scm
(load "../test-framework.scm")
(load "set.scm")
(load "../lib.scm")

;;;
(set! test-section "element-of-set?: ")

(test-eq "simple #t" (element-of-set? 'x '(a b x)) #t)

(test-eq "simple #f" (element-of-set? 'x '(a b)) #f)
(test-eq "single nest #f" (element-of-set? 'x '(a b (x))) #f)
(test-eq "single nest #t" (element-of-set? '(x) '(a b (x))) #t)

(test-eq "multi nest #f" (element-of-set? '((x)) '(a b ((x)))) #t)

;;;
(set! test-section "equal-set?: ")

(test-eq "simple"
  (equal-set? '(a b c)
              '(b c a))
  #t)
(test-eq "complex"
  (equal-set? '(a b ((c)) (b) d e f)
              '(b a (b) d e ((c)) f))
  #t)

;;;
(set! test-section "adjoin-set: ")

(define s '(a b c))
(define adjoined (adjoin-set 'd s))
(test-eq "d to abc #f" (element-of-set? 'd s) #f)
(test-eq "d to abc #t" (element-of-set? 'd adjoined) #t)

(define s '(a (b) ((c))))
(define adjoined (adjoin-set 'a s))
(test-eq "nested 1" (element-of-set? 'a s) #t)
(test-eq "nested 2" (element-of-set? 'a adjoined) #t)

(define s '(a (b) ((c))))
(define adjoined (adjoin-set '((c)) s))
(test-eq "nested 3" (element-of-set? '((c)) s) #t)
(test-eq "nested 4" (element-of-set? '((c)) adjoined) #t)

(define s '(a (b) ((c))))
(define adjoined (adjoin-set '(c) s))
(test-eq "nested 5" (element-of-set? '(c )s) #f)
(test-eq "nested 6" (element-of-set? '(c) adjoined) #t)

;;;
(set! test-section "intersection-set: ")

(define p '(a b c))
(define q '(d e f))
(test-eq "empty" (intersection-set p q) '())

(define p '(a b c))
(define q '(a b d e f))
(test-eq "a b" (intersection-set p q) '(a b))

(define p '(a b ((c))))
(define q '(a (b) d e ((c)) f))
(test-eq "nested a b" (intersection-set p q) '(a ((c))))

;;;
(set! test-section "union-set: ")

(define p '(a b ((c))))
(define q '(a (b) d e ((c)) f))
(test-eq "complex"
  (equal-set? (union-set p q)
              '(a b ((c)) (b) d e f))
  #t)


