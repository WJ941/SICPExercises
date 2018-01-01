(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-cdr s))
        (stream-for-each proc (stream-cdr s))
        )
  )
)
(define (display-stream s)
        (stream-for-each (lambda (x) (display-line x)) s)
)
(define (average a b) (/ (+ a b) 2))
(define (sqrt-improve x guess)
        (average guess (/ x guess))
)
(define (sqrt-stream x)
        (define guesses (cons-stream 1.0
            (stream-map (lambda (guess) (sqrt-improve x guess)) guesses))
        )
        guesses
)
(display-stream (sqrt-stream 3))