;;;; Miscellaneous procedures and notes
;;;;
;;;; Don't load this file

;; Read time conditional's
(define foo
  (lambda (x)
    (cond
     ((zero? x) 0)
     ((= 1 x) 1)
     #;(else x))))

;; Example indentation when operator is a list
(define bar
; use first form if line must be slit
  (lambda (ls)
      ((car ls)
       (cdr ls)
       foo)))
;; default Emacs indentation
;      ((car ls) (cdr ls)
;       foo)))
;
;; this would be nice also though (default Emacs does not do this)
;      ((car ls) (cdr ls)
;                foo)))

;; Indentation style for strings

  Acceptable:

    ("foo" "bar" "baz" "quux" "zot"
     "mumble" "frotz" "gargle" "mumph")

    ("foo"
     "bar" "baz" "quux" "zot"
     "mumble" "frotz" "gargle" "mumph")


;;;; Miscellaneous procedures and notes
;;;;
;;;; Don't load this file

;; Read time conditional's
(define foo
  (lambda (x)
    (cond
     ((zero? x) 0)
     ((= 1 x) 1)
     #;(else x))))

;; Example indentation when operator is a list
(define bar
; use first form if line must be slit
  (lambda (ls)
      ((car ls)
       (cdr ls)
       foo)))
;; default Emacs indentation
;      ((car ls) (cdr ls)
;       foo)))
;
;; this would be nice also though (default Emacs does not do this)
;      ((car ls) (cdr ls)
;                foo)))

;; Indentation style for strings

  Acceptable:

    ("foo" "bar" "baz" "quux" "zot"
     "mumble" "frotz" "gargle" "mumph")

    ("foo"
     "bar" "baz" "quux" "zot"
     "mumble" "frotz" "gargle" "mumph")

