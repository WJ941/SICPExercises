(define (divide-stream s1 s2)
  (stream-map / s1 s2)
)
(define (integrate-series s)
  (cons-stream (stream-cdr s)
    (divide-stream (stream-cdr s) (stream-cdr integers)))
)
(define successive
  (cons-stream 1 (mul-streams (stream-cdr (integrate-series ones))
                            cosx
  ))
)
(define zero-one 
  (cons-stream 0 (stream-map 
                  (lambda (x)
                    (cond ((= 0 x) 1)
                          ((= 1 x) 0)
                    ))
                  zero-one))
  )

(define one-zero
  (stream-cdr zero-one)
  )
(define successive-even 
  (mul-streams successive zero-one)
  )
(define successive-odd
    (mul-streams successive one-zero)
  )
(define zero-negative
  (stream-map
        (lambda (x)
          (cond ((odd? x) 0)
                ((odd? (/ x 2)) -1)
                ((even? (/ x 2)) 1)
          )
        )
        integers
  ))

(define one-zero-negative
    (cons-stream 1 zero-negative)
  )
(define cosine-series
  (cons-stream 1 (mul-streams
      (stream-cdr successive-even)
      (stream-cdr zero-negative)
    ))
  )

(define sine-series
  (cons-stream 0 (mul-streams one-zero-negative successive-odd))
)