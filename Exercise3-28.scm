(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
          (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
      (logical-or (get-signal a1) (get-signal a2))))
  (after-delay or-gate-delay (lambda () (set-signal! output new-value)))))

  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
'ok)