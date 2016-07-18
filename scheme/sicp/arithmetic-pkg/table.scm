;;;; Tables

;;; http://community.schemewiki.org/?sicp-ex-3.25

(define (fold-left op init seq) 
  (define (iter ans rest) 
    (if (null? rest) 
        ans 
        (iter (op ans (car rest)) 
              (cdr rest)))) 
  (iter init seq)) 

(define (make-table same-key?) 
  (define (associate key records) 
    (cond ((null? records) #f) 
          ((same-key? key (caar records)) (car records)) 
          (else (associate key (cdr records))))) 
  
  (let ((the-table (list '*table*))) 
    (define (lookup keys) 
      (define (lookup-record record-list key) 
        (if record-list 
            (let ((record (associate key record-list))) 
              (if record 
                  (cdr record) 
                  #f)) 
            #f)) 
      (fold-left lookup-record (cdr the-table) keys)) 
    
    (define (insert! keys value) 
      (define (descend table key) 
        (let ((record (associate key (cdr table)))) 
          (if record 
              record 
              (let ((new (cons (list key) 
                               (cdr table)))) 
                (set-cdr! table new) 
                (car new))))) 
      (set-cdr! (fold-left descend the-table keys) 
                value)) 
    
    ;; N.B. PRINT will break if a record has a list structure for a value. 
    (define (print) 
      (define (indent tabs) 
        (for-each (lambda (x) (display #\tab)) (iota tabs))) 
      
      (define (print-record rec level) 
        (indent level) 
        (display (car rec)) 
        (display ": ") 
        (if (list? rec) 
            (begin (newline) 
                   (print-table rec (1+ level))) 
            (begin (display (cdr rec)) 
                   (newline)))) 
      
      (define (print-table table level) 
        (if (null? (cdr table)) 
            (begin (display "-no entries-") 
                   (newline)) 
            (for-each (lambda (record) 
                        (print-record record level)) 
                      (cdr table)))) 
      
      (print-table the-table 0)) 
    
    (define (dispatch m) 
      (cond ((eq? m 'lookup) lookup) 
            ((eq? m 'insert!) insert!) 
            ((eq? m 'print) print) 
            (else (error "Undefined method" m)))) 
    
    dispatch))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;;; One Dimensional Procedural Implementation

(define (make-table-1d)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;; Two Dimensional Procedural Implementation

(define (make-table-2d)
  (let ((local-table (list '*table*)))
    (define (lookup op type)
      (let ((subtable (assoc op (cdr local-table))))
        (if subtable
            (let ((record (assoc type (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! op type item)
      (let ((subtable (assoc op (cdr local-table))))
        (if subtable
            (let ((record (assoc type (cdr subtable))))
              (if record
                  (set-cdr! record item)
                  (set-cdr! subtable
                            (cons (cons type item)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list op
                                  (cons type item))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
