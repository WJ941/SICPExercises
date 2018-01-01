(define (pi-submands n)
  (cons-stream (/ 1.0 n)
    (stream-map - (pi-submands (+ n 2)))
  )
)
(define (partial-sums s)
  (define sums
    (cons-stream (stream-car s)
      (add-stream (stream-cdr s) sums)
    )
  )
  sums
)
(define pi-stream 
  (scale-stream (partial-sums (pi-submands 1)) 4)
  )
(display-stream pi-stream)