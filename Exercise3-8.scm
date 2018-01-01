(define a #t)
(define (f m)
  (cond (a (set! a #f) m)
        (else (set! a #f) 0)
  )
  
)