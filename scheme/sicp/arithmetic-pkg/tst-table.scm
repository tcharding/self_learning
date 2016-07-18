;;;; tests for table.scm
(load-from-path "test-framework.scm")
(load-from-path "lib.scm")
(load-from-path "arithmetic-pkg/table.scm")

(define t1 (make-table-1d))             ; one dimensional
((t1 'insert!) 'a 1)
((t1 'insert!) 'b 2)

(test-eq "" ((t1 'lookup) 'a) 1)
(test-eq "" ((t1 'lookup) 'b) 2)

(define t2 (make-table-2d))             ; two dimensional
((t2 'insert!) 'foo 'a 1)
((t2 'insert!) 'bar 'a 2)
((t2 'insert!) 'foo 'b 3)
((t2 'insert!) 'bar 'b 4)

(test-eq "" ((t2 'lookup) 'foo 'a) 1)
(test-eq "" ((t2 'lookup) 'bar 'a) 2)
(test-eq "" ((t2 'lookup) 'foo 'b) 3)
(test-eq "" ((t2 'lookup) 'bar 'b) 4)

(define t (make-table eqv?))                 ; arbitrary dimensions
(define get (t 'lookup))
(define put (t 'insert!))

(put '(a) 1)
(put '(b) 2)
(test-eq "" ((t 'lookup) '(a)) 1)
(test-eq "" ((t 'lookup) '(b)) 2)

(put '(foo a) 1)
(put '(bar a) 2)
(put '(foo b) 3)
(put '(bar b) 4)
(test-eq "" (get '(foo a)) 1)
(test-eq "" (get '(bar a)) 2)
(test-eq "" (get '(foo b)) 3)
(test-eq "" (get '(bar b)) 4)

(put '(one (two)) 1)
(test-eq "nested" (get '(one (two))) 1)

(put '(one (two (thee)) (four)) 1)
(test-eq "more nested" (get '(one (two (thee)) (four))) 1)

