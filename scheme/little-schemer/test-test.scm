(load "test-manager/load.scm")

                                        ; This is a test group named simple-stuff
(in-test-group
 simple-stuff

                                        ; This is one test named arithmetic
 (define-test (arithmetic)
   "Checking that set! and arithmetic work"
   (define foo 5)
   (check (= 5 foo) "Foo should start as five.")
   (set! foo 6)
   (check (= 36 (* foo foo))))

                                        ; Each of these will become a separate anonymous one-form test
 (define-each-test
   (check (= 4 (+ 2 2)) "Two and two should make four.")
   (check (= 2147483648 (+ 2147483647 1)) "Addition shouldn't overflow."))

                                        ; Each of these will become a separate anonymous one-form test using check
 (define-each-check
   (= 6 (+ 2 2 2))
   (equal? '(1 2 3) (cons 1 '(2 3))))

                                        ; This is a test that looks like a REPL interaction
 (define-test (interactive)
   (interaction
    (define foo 5)
    foo
    (produces 5)  ; This compares against the value of the last form
    (set! foo 6)
    (* foo foo)
    (produces 36))))

(run-registered-tests)

                                        ; Can run individual groups or tests with
(run-test '(simple-stuff))
(run-test '(simple-stuff arithmetic))
