;;;; A Simulator for Digital Circuits
;;;;
;;;; SICP, section 3.3.4

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (hald-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      ((after-delay inverter-delay
                    (lambda ()
                      (set-signal! output new-value))))))
  (add-action! input invert-input)
  'ok)

;;; logical operators

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1))
      1
      0))

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1))
      1
      0))

(define (xor s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 0)
        ((or (= s1 1) (= s2 1)) 1)
        (else 0)))

;;; simple gates

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;;; Exercise 3.29

;; delay = 2 * inverter-delay + and-gate-delay
(define (or-gate a1 a2 output)
  "alternate implementation"
  (let ((b1 (make-wire))
        (b2 (make-wire))
        (out-1 (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 out-1)
    (inverter out-1 output))
  'ok)
    
;;; Exercise 3.30

(define (ripple-carry-adder ls-a ls-b ls-s c)
  
