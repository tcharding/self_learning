;;;; Circuit Simulation Agenda

(load-from-path "circuits/queue.scm")

;;; Time segment representation

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s)
  (car s))

(define (segment-queue s)
  (cdr s))

;;; Agenda representation

(define (make-agenda)
  (list 0))

(define (current-time agenda)
  (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda)
  (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

;;; Agenda procedures

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM" agenda)
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (remove-first-agenda-item! agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- REMOVE-FIRST-AGENDA-ITEM!" agenda)
      (let ((q (segment-queue (first-segment agenda))))
        (delete-queue? q)
        (if (empty-queue? q)
            (set-segments! agenda (rest-segments agenda))))))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-car! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

