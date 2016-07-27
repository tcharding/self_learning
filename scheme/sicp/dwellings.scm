;;;; Logic Puzzle
;;;;
;;;; SICP page 418,
;;;;  from 'Superior Mathematical Puzzles - Howard Dines man'
;;;;
;;;; Exercise 4.41

(load-from-path "lib.scm")

;;; Represent dwelling arrangement as a set of characters
;;; (first letter of persons name) eg '(b c m f s)
;;;
;;; 1st floor baker
;;; 2nd floor cooper
;;; 3rd floor miller
;;; 4th floor fletcher
;;; 5th floor smith

(define (multiple-dwelling)
  (print-dwellings
   (filter
    baker-not-5
    (filter
     cooper-not-1
     (filter
      fletcher-not-1-or-5
      (filter
       miller-gt-cooper
       (filter
        smith-fletcher-not-adjacent
        (filter
         fletcher-cooper-not-adjacent
         (all-dwelling-sequences)))))))))

(define (print-dwellings dwellings)
  (display dwellings)
  (newline))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;;; Predicates

(define (baker-not-5 seq)
  (not (eq? 'b (list-ref seq 4))))

(define (cooper-not-1 seq)
  (not (eq? 'c (list-ref seq 0))))

(define (fletcher-not-1-or-5 seq)
  (and (not (eq? 'f (list-ref seq 0)))
       (not (eq? 'f (list-ref seq 4)))))

(define (miller-gt-cooper seq)
  (> (position 'm seq) (position 'c seq)))

(define (fletcher-cooper-not-adjacent seq)
  (not-adjacent 'f 'c seq))

(define (smith-fletcher-not-adjacent seq)
  (not-adjacent 's 'f seq))

(define (position who seq)
  (let iter ((pos 1) (remaining seq))
    (cond ((null? seq)
           (error "no present -- POSITION" who))
          ((eq? who (car remaining)) pos)
          (else (iter (inc pos) (cdr remaining))))))

(define (not-adjacent w1 w2 seq)
  (let ((p1 (position w1 seq))
        (p2 (position w2 seq)))
    (and (not (= (dec p1) p2))
         (not (= (dec p2) p1)))))
           

;;; Generate sequences

(define (all-dwelling-sequences)
  (permutate-perms
   's
   (permutate-perms
    'm
    (permutate-perms
     'f
     (permutate-perms
      'c '((b)))))))

(define (permutate-perms item perms)
  "return all permutations of item and perms"
  (if (null? perms)
      '()
      (append (permutate-list item (car perms))
              (permutate-perms item (cdr perms)))))

(define (permutate-list item seq)
  "returns all ways of adding item to seq, members of seq maintain order"
  (let iter ((front '()) (back seq) (perms '()))
    (cond ((null? back)
           (cons (append front (list item)) perms))
          (else (iter
                 (append front (list (car back)))
                 (cdr back)
                 (cons (append front (cons item back))
                       perms))))))
   
