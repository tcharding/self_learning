;;;; Scheme Test Framework

(define (test-eq msg got exp)
  (unless (equal? got exp)
    (fail msg got exp)))

(define fudge 0.001)
(define (test-feq msg got exp)
  "float equivalence"
  (unless (< (abs (- got exp)) fudge)
    (fail msg got exp)))

(define (fail msg got exp)
  (display "Test Failed: ")
  (display msg)
  (newline)
  (display "got: ")
  (display got)
  (newline)
  (display "exp: ")
  (display exp)
  (newline))


