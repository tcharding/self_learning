;;;; Ease debugging interpreter using guile in the shell
;;;;
;;;; Only load this once per guile session


(define apply-in-underlying-scheme apply)

(load-from-path "lib.scm")


(define (ld)
  (load-from-path "streams-lazy-interpreter.scm"))
;  (load-from-path "lazy-memo-interpreter.scm"))

(define (dr)
  (driver-loop))
