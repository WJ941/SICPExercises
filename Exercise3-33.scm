(load "constrantsys.scm")
(define (average-converter a b c)
  (let ((v (make-connector))
        (w (make-connector))
      )
      (multiplier c v w)
      (adder a b w)
      (constant 2 v)
      'ok
  )
)
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(average-converter a b c)
(probe "factor a" A)
(probe "factor b" B)
(probe "half average" C)