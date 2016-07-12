;;;; Section 2.3.3 Representing Sets

;;; Represent a set as a binary tree without repetition
;;;
;;; Only supports integers
(use-modules (ice-9 debug))

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (union-set p q)
  (list->tree (merge-ordered-lists (tree->list p)
                                   (tree->list q))))

(define (merge-ordered-lists l m)
  (cond ((null? l) m)
        ((null? m) l)
        (else (let ((x1 (car l)) (x2 (car m)))
                  (cond ((= x1 x2) (cons x1 (merge-ordered-lists (cdr p) (cdr q))))
                        ((< x1 x2) (cons x1 (merge-ordered-lists (cdr p) q)))
                        (else (cons x2 (merge-ordered-lists p (cdr q)))))))))

                (cond (= xl xm)

; cannot think of a way to do this short of converting trees into list?                      
(define (intersection-set p q)
  #f)

(define (equal-set? p q)
  (equal? (tree->list p) (tree->list q)))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
                            
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts ((cdr right-result))))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts)))))))
  
                             
