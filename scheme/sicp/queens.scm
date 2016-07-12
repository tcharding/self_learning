;;;; 8 Queens Puzzle
;;;;
;;;; Place 8 queens on a chess-board so that no two are on the same row, column or diagonal
(load "lib.scm")
(load "signal.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())
(define (col k board)
  (list-ref board (dec k)))

(define (adjoin-position row column board)
  (append board (list row)))

(define (safe? k board)
  "determine whether queen in column k is safe"
  (and (safe-rows? k board)
       (safe-diagonals? k board)))

(define (safe-rows? k board)
  (not (contains-duplicates? board)))

(define (safe-diagonals? k board)
  (if (= 1 k)
      #t
      (and
       (not (rows-same-diagonal-using-distance? (car board) (col k board) (dec k)))
       (safe-diagonals? (dec k) (cdr board)))))

(define (contains-duplicates? ls)
  (cond ((null? ls) #f)
        ((member? (car ls) (cdr ls)) #t)
        (else (contains-duplicates? (cdr ls)))))
  
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
  
