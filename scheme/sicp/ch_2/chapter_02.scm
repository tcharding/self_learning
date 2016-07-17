;;;; Various exercises from chapter 2
;;;;
(load "../lib.scm")
(load "signal.scm")
(load "../ch_1/primes.scm")

;;; Church Numerals
(define (zero f)
    (lambda (x) x))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
 
;;;; Exercise 2.6

(define (one f)
  (lambda (x)
    (f x)))

(define (two f)
  (lambda (x)
    (f (f x))))

;;;; Exercise 2.17

(define (last-pair ls)
  (if (null? (cdr ls))
      (car ls)
      (last-pair (cdr ls))))

;;;; Exercise 2.18

(define (reverse ls)
  (let fn ((ls ls) (rev '()))
    (if (null? ls)
        rev
        (fn (cdr ls) (cons (car ls) rev)))))

;;;; Exercise 2.19

(define (cc amount coin-values)
  "Count ways of making up amount from list of coins"
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;;;; Exercise 2.20

(define (same-parity initial . nums)
  (reverse
   (let fn ((nums nums) (same (list initial)))
     (cond ((null? nums) same)
           (else (if (is-same-parity initial (car nums))
                     (fn (cdr nums) (cons (car nums) same))
                     (fn (cdr nums) same)))))))

(define (is-same-parity n m)
  (or (and (even? n) (even? m))
      (and (odd? n) (odd? m))))

;;;; Exercise 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square x)
  (* x x))

(define (square-list items)
  (map square items))

;;;; Exercise 2.23

(define (for-each proc ls)
  (cond ((null? ls) #t)
        (else (begin
                (proc (car ls))
                (for-each proc (cdr ls))))))

;;;; Exercise 2.27

(define (deep-reverse ls)
  (let fn ((ls ls) (rev '()))
    (cond ((null? ls) rev)
          ((atom? (car ls)) (fn (cdr ls) (cons (car ls) rev)))
          (else (fn (cdr ls) (cons (deep-reverse (car ls)) rev))))))

(define (atom? x)
  (not (pair? x)))
                        
;;;; Exercise 2.28

(define (fringe ls)
  "flatten ls"
  (cond ((null? ls) '())
        ((atom? (car ls)) (cons (car ls) (fringe (cdr ls))))
        (else (append (fringe (car ls)) (fringe (cdr ls))))))

;;;; Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
         
;;;; Exercise 2.31

(define (square-tree tree)
  (tree-map square tree))

(define (tree-map proc tree)
    (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map square (car tree))
                    (tree-map square (cdr tree))))))

;;;; Exercise 2.32

(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;;;; Exercise 2.33
#!
(define (map p sequence)
  (accumulate (lambda (seqn so-far)
                (cons (p seqn) so-far))
                '()
                sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
              
(define (length sequence)
  (accumulate + 0 (map (lambda (x) 1) sequence)))
              
(define (length sequence)
  (accumulate (lambda (ignored-element tally) (inc tally)) 0  sequence))
!#
;;;; Exercise 2.34

(define (horner-eval x coefficient-sequence)
  ; div x because we don't want to mul by x for last (constant) coefficient
  (/ (accumulate (lambda (this-coeff higher-terms)
                (* (+ this-coeff higher-terms) x))
              0
              coefficient-sequence)
     x))                                

;;;; Exercise 2.35

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree tree))))

;;;; Exercise 2.36

; see signal.scm: accumulate-n

;;;; Exercise 2.38

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;;;; Exercise 2.39

(define (reverse sequence)
  (fold-right (lambda (a ls)
                (push a ls))
              '()
              sequence))

(define (push a ls)
  "push a onto end of ls"
  (if (null? ls)
      (list a)
      (cons (car ls) (push a (cdr ls)))))

(define (reverse sequence)
  (fold-left (lambda (a ls)
               (cons ls a))
             '()
             sequence))

;;;; Nested mapping
;;;;
;;;; Given positive integer n, find all ordered pairs of distinct
;;;; positive integers i and j, where 1 <= i < j <= n, 
;;;; such that i + j is prime

;; my version
(define (descriptive-procedure-name n)
  (filter descending?
          (filter prime-sum?
                  (distinct (gen-sequence n)))))
;; sicp's version
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (dec i))))
                        (enumerate-interval 1 n)))))

(define (descending? ls)
  (not (ascending? ls)))
  
(define (prime-sum? ls)
  (prime? (sum ls)))

(define (distinct ls)
  (cond ((null? ls) '())
        ((member? (car ls) (cdr ls)) (distinct (cdr ls)))
        (else (cons (car ls)
                    (distinct (cdr ls))))))

;; initial attempt, see below for improvements
(define (gen-sequence n)
  "generate list of all pairs such that 1 <= i < j <= n"
  (let outer ((i 2) (seq-outer '()))
    (if (<= i n)
        (outer (inc i) (append (let inner ((j 1) (seq-inner '()))
                               (if (<= j n)
                                   (inner (inc j) (cons (list i j) seq-inner))
                                   seq-inner))
                             seq-outer))
        seq-outer)))

(define (gen-sequence n)
  "as above but using accumulate"
  (accumulate append
              '()
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (dec i))))
                     (enumerate-interval 1 n))))
                  
(define (ascending? ls)
  (cond ((null? ls) #t)
        ((null? (cdr ls)) #t)
        (else (and (< (car ls) (cadr ls))
                   (ascending? (cdr ls))))))

(define (member? a ls)
  (cond ((null? ls) #f)
        ((equal? a (car ls)) #t)
        (else (member? a (cdr ls)))))
                
(define (sum ls)
  (accumulate + 0 ls))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (gen-sequence n)
 (flatmap (lambda (i)
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 (dec i))))
          (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair)))

(define (permutations set)
  (if (null? set)
      (list '()))
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x set))))
               set))

(define (remove item sequence)
  (filter (lambda (x)
            (not (equal? x item)))
          sequence))

;;;; Exercise 2.40

(define (unique-pairs n)
  "generate sequence of pairs (i j) where 1 <= j < i <= n"
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval (inc i) n)))
           (enumerate-interval 1 n)))
                              
(define (prime-sum-pairs n)
  "simplified using unique-pairs"
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (triples-sum-to-less-than s n)
  "all triples (i j k) where i + j + k = s and i, j, k are less than s"
  (distinct (ordered-triples (filter (lambda (trip)
                              (= s (sum trip)))
                            (all-triples n)))))

(define (all-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval 1 (dec n))))
                  (enumerate-interval 1 (dec n))))
           (enumerate-interval 1 (dec n))))

(define (all-pairs n)
  "generate sequence of pairs (i j) where 1 <= [ij] < n"
  (flatmap (lambda (i)
         (map (lambda (j) (list i j))
              (enumerate-interval 1 (dec n))))
         (enumerate-interval 1 (dec n))))

(define (ordered-triples sequence)
  (if (null? sequence)
     '()
     (cons (sort (car sequence) <) (ordered-triples (cdr sequence)))))

;;;; Exercise 2.54

(define (*equal? s-exp1 s-exp2)
  (cond ((and (null? s-exp1) (null? s-exp2))
         #t)
        ((and (number? s-exp1) (number? s-exp2))
         (= s-exp1 s-exp2))
        ((and (symbol? s-exp1) (symbol? s-exp2))
         (eq? s-exp1 s-exp2))
        (else (and (*equal? (car s-exp1) (car s-exp2))
                   (*equal? (cdr s-exp1) (cdr s-exp2))))))

(define (*symbol? s-exp)
  (not (or (number? s-exp)
           (list? s-exp))))

(define (*list? s-exp)
  (cond ((null? s-exp) #t)
        ((pair? s-exp)
         (list? (cdr s-exp)))
        (else #f))) 

;;; Exercise 2.66

; database is implemented as a binary tree
(define (lookup key db)
  (cond ((null? db) #f)
        ((= key (key db)) #t)
        ((< key (key db))
         (lookup key (left-branch db)))
        ((> key (key db))
         (lookup key (right-branch db)))))


   

