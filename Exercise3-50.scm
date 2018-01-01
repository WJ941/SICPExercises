(load "stream.scm")
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      '()
      (cons-stream
      (apply proc (map (lambda (s) (stream-car s)) argstreams))
      (apply stream-map
            (cons proc (map (lambda (s) (stream-cdr s)) argstreams))))))


(define (show x) (display-line x) x)

(define x (stream-map show
  (stream-enumerate-interval 0 10)))
 (stream-ref x 5)
 (stream-ref x 7)
