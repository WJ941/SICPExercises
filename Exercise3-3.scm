(define (make-account balance pwd)
  (let ((wrong-times 0))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds")
    )
    (define (deposit amount)
      (begin (set! balance (+ balance amount)) balance)
    )
    (define (wrong-pwd)
      (if (> wrong-times 7)
          (error "call-the-cops")
          (error "Incorrect-password")
      )
    )
    (define (dispatch password m)
      (cond ((eq? password pwd)
              (set! wrong-times 0)
              (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknow request: MAKE-ACCOUTN" m))
              )
            )
          (else
            (set! wrong-times (+ wrong-times 1))
            (wrong-pwd)
          )
      )
    )
    dispatch
  )
)