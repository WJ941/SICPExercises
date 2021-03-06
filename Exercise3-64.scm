(define (stream-limit stream tolerance)
  (define (iter stream)
    (let ((s0 (stream-car stream))
          (s1 (stream-car (stream-cdr stream)))
        )
        (if (< (abs (- s1 s0)) tolerance)
            s1
          (iter (stream-cdr stream))
        )
  )
  )
  (iter stream)
)
(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance)
)