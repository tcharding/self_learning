;;;; tests for set-tree.scm
(load "../test-framework.scm")
(load "set-tree.scm")
(load "../lib.scm")

(use-modules (ice-9 debug)
             (system vm trace))

(define (make-leaf value)
  (make-tree value '() '()))

(define t1 (make-tree 7
                      (make-tree 3 (make-leaf 1) (make-leaf 5))
                      (make-tree 9 '() (make-leaf 11))))
                                 
                                 
