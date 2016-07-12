;;;; Section 2.3.4 Huffman Encoding Trees
;;;;
;;;; Nodes can be either 'leaf' nodes or 'code-tree' nodes.
(load "lib.scm")

;;; Constructors

(define (make-leaf symbol weight)
  "leaf node"
  (list 'leaf symbol weight))

(define (make-code-tree left right)
  "code tree node"
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;;; Selectors

(define (symbol-leaf leaf)
  (cadr leaf))

(define (weight-leaf leaf)
  (caddr leaf))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;; Predicates

(define (leaf? obj)
  (cond ((null? obj) #f)
        ((eq? (car obj) 'leaf) #t)
        (else #f)))

;;;

(define (decode bits tree)
  "bits to message (symbols)"
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  "message (symbols) to bits"
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
               (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  "encode single symbol to bits"
  (define (encode-symbol-1 sym current-branch)
    (cond ((leaf? current-branch)
           '())
          ((in-left-branch? sym current-branch)
           (cons 0 (encode-symbol-1 sym (left-branch current-branch))))
          (else                         ; in right-branch
           (cons 1 (encode-symbol-1 sym (right-branch current-branch))))))
  (if (member? sym (symbols tree))
      (encode-symbol-1 sym tree)
      (error "unknown symbol -- ENCODE-SYMBOL" sym)))

(define (in-left-branch? sym tree)
  (member? sym (symbols (left-branch tree))))

(define (in-right-branch? sym tree)
  (member? sym (symbols (right-branch tree))))

(define (generate-huffman-tree pairs)
  "pairs is list of form ((symbol weight) ...)
    list is ordered by weight (biggest first)" 
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-pairs)
  (if (= 1 (length leaf-pairs))
      (error "merge requires 2 or more pairs" leaf-pairs)
      (let iter ((set leaf-pairs))
        (if (= 1 (length set))
            (car set)
            (iter (adjoin-set
                   (make-code-tree (cadr set) (car set))
                   (cddr set)))))))

(define (make-leaf-set pairs)
  "returns leaf set in ascending order"
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ; symbol
                                 (cadr pair)) ; frequency
                      (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((<= (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
