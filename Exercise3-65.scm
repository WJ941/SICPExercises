(define (ln2-submands n)
  (cons-stream (/ 1.0 n)
    (stream-map - (ln2-submands (+ n 1)))
  )
)

(define ln2-stream
  (partial-sums (ln2-submands 1))
)
(display-stream (accelerated-sequence euler-transform ln2-stream))
