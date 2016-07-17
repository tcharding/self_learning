;;;; Sequences as Conventional Interfaces
;;;;
;;;; Section 2.2.3
(load "../lib.scm")

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (inc low) high))))
         
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((atom? tree) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (firsts seqs))
            (accumulate-n op init (remove-firsts seqs)))))

(define (firsts seqs)
  (if (null? seqs)
      '()
      (cons (caar seqs) (firsts (cdr seqs)))))

(define (remove-firsts seqs)
  (if (null? seqs)
      '()
      (cons (cdar seqs) (remove-firsts (cdr seqs)))))
  

;;;; Example usage

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons '() (filter even? (map fib (enumerate-interval 0 n)))))

(define (square x)
  (* x x))

;;;; Fibonacci (from chapter_01.scm)
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q))
		   (+ (* 2 (* p q)) (square q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))
