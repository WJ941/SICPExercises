(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (lambda ()
       expr))))

(define-syntax cons-stream
  (syntax-rules ()
    ((stream a b)
     (cons a (delay b)))))


(define (force x)
(x))

(define (stream-car s)
(car s))

(define (stream-cdr s)
  (force (cdr s)))
(define (stream-filter pred s)
      (cond ((stream-null? s) the-empty-stream)
            ((pred (stream-car s))
                (cons-stream (stream-car s)
                  (stream-filter pred (stream-cdr s))
                )
            )
            (else (stream-filter pred (stream-cdr s)))
      )
)
(define (integer-starting-from n)
  (cons-stream n
     (integer-starting-from (+ n 1))
  )
)
(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
    (cons-stream (stream-car stream)
      (sieve 
          (stream-filter 
            (lambda (x) (not (divisible? x (stream-car stream))))
            (stream-cdr stream))        )
    )
)
(define primes
  (sieve (integer-starting-from 2))  
)