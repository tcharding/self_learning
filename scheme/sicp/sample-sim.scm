;;;; Circuits Sample simulation

(load-from-path "circuits.scm")
(load-from-path "agenda.scm")

;;; Initialize and configure simulation

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;;; Simulation specific procedures

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda)
                     action
                     the-agenda)))

(define (propogate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agend-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propogate))))

;;; Setup Simulation

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(hald-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propogate)

(set-signal! input-2 1)
(propogate)


