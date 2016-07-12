;;;; 8 Queens Puzzle
;;;;
;;;; Place 8 queens on a chess-board so that no two are on the same row, column or diagonal
(load "lib.scm")
(load "signal.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (empty-board board-size)
;        (filter
;         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position (dec k) new-row rest-of-queens))
                 (enumerate-interval 0 (dec board-size))))
          (queen-cols (- k 1)))))
  (queen-cols board-size))

;;;; Board Constructor

(define %empty -1)

(define (empty-board n)
  (if (zero? n)
      '()
      (cons %empty (empty-board (dec n)))))

;;;; Board Selectors

(define (set-column-row col row board)
  "set column col to row"
  (if (= col 0)
      (cons row (cdr board))
      (cons (car board) (set-column-row (dec col) row (cdr board)))))

(define (clear-column col board)
  (set-column-row col %empty board))

(define (column-clear? col board)
  (= (list-ref board col) %empty))

;;;;

(define (adjoin-position col row board)
  (if (not (column-clear? col board))
      (av 'adjoin-position "column not free" col row board)
      (set-column-row col row board)))
  
(define (safe? k board)
  "determines whether queen in column k is safe"
  (and (rows-safe? k board)
       (diagonals-safe? k board)))

(define (rows-safe? k board)
  (let ((row-val (list-ref board k)))
    (if (column-clear? k board)
        #t
        (not (member? row-val (clear-column k board))))))

(define (diagonals-safe? k board)
  (if (zero? k)
      #t
      (and
       (not (rows-same-diagonal-using-distance? (car board) (list-ref board k) k))
       (diagonals-safe? (dec k) (cdr board)))))

(define (member? element list)
  (cond ((null? list) #f)
        ((equal? (car list) element) #t)
        (else (member? element (cdr list)))))

(define (rows-same-diagonal-using-distance? row1 row2 distance)
  "determines whether two rows distance apart are on same diagonal"
  (if (or (= -1 row1)
          (= -1 row2))
      #f
      (or (= row1 (+ distance row2))
          (and (>= (- row2 distance) 0)
               (= row1 (- row2 distance))))))
