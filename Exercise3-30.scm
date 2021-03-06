(define (ripple-carry-adder list-A list-B list-S wire-C)
  (define c-n (make-wire)) ; c-n = 0
  (define (iter listA listB listS c-in)
    (cond ((= 1 (length list-A))
            (full-adder (car listA) (car listB) c-in (car listS) wire-C))
          (else
            (let ((c-out (make-wire)))
              (full-adder (car listA) (car listB) c-n (car listS) c-out)
              (iter (cdr listA) (cdr listB) (cdr listS) c-out)
            )
          )
    )
  )
  (iter list-A list-B list-S c-n)
)