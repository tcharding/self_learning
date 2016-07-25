;;;; tests for streams.scm
(load-from-path "test-framework.scm")
(load-from-path "streams.scm")
(load-from-path "lib.scm")

(use-modules (ice-9 streams))

(define odds (make-stream (lambda (state)
                            (cons state (+ state 2)))
                          1))

(define (square n) (* n n))
(define oddsquares (stream-map square odds))

;(stream-car odds)
;(stream-car (stream-cdr odds))


#!
(display-stream (stream-map (lambda (x) x) (stream-enumerate-interval 1 10)))

;
!#
