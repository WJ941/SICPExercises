Exercise 1.2:
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
Exercise 1.3:
(define (f x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        ((and (< z x) (< z y)) (sum-of-squares x y))
        (else (sum-of-squares x y))
  )
)
   
(define (sum-of-squares x y)
  (+ (square x) (square y))
)
(define (square x) (* x x))
(define (max x y) 
  (if (> x y)
    x
    y)
)
Exercise 1.9
first: recuisive
second: iterative

Exercise 1.10
(A 1 10) => 1024
(A 2 4) => 65536

Exercise 1.11
recursive:
(define (f n) 
  (if (< n 3)
    n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)) ))
  )
)
iterative:
(define (f n) 
  (f-iter 0 1 2 n)
)
    
(define (f-iter a b c count)
  (if (< count 3)
      c
      (f-iter b c (+ (* 3 a) (* 2 b) c) (- count 1))
  )
)
      
Exercise 1.12
(define (f a b)
  (cond ((or (= a b) (= b 1)) 1)
    (else 
      (+ (f (- a 1) (- b 1)) (f (- a 1) b))
    )
  )
)
Exercise 1.15
a: 2^5 + 1 = 33

Exercise 1.16
(define (fast-expt b n) (fast-expt-iter b 1 n)
)

(define (fast-expt-iter b a n)
  (cond ((or (= n 0) (= n 1)) a)
    ((iseven n) (fast-expt-iter b (* a (square b)) (/ n 2)))
    (else (fast-expt-iter b (* a b) (- n 1)))
  )
)
(define (square x)
  (* x x)
)
(define (iseven n)
(= (remainder n 2) 0))

Exercise 1.17
(define (iseven n)
(= (remainder n 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (* a b)
  (cond ((or (= b 0) (= a 0)) 0)
    ((= b 1) a)
    ((iseven b) (* (double a) (halve b) ))
    (else (+ a (* (double a) (halve (- b 1)))))
  )
)

Exercise 1.29
(define (integral f a b n) 
  (define h (/ (- b a) n))
  (define (addx x)
    (+ x h h)
  )

  (* (sum f a addx b)
    (/ h 3)
  )
)
(define (sum term a next b) 
  (if (> a b)
    0
    (+ (term a) (* 4 (term (/ (+ (next a) a) 2))) (term (next a))
       (sum term (next a) next b)
    )
  )
)
(define (cube x) (* x x x))
(integral cube 0 1 100)

Exercise 1.30
(define (sum term a next b) 
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a))
    )
  )
  (iter a 0)
)

Exercise 1.31
iterative:
(define (product term a next b) 
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))
    )
  )
  (iter a 1)
)
recursive:
(define (product term a next b) 
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b)
    )
  )
)
factorial:
(define (fac n)
  (product f 1 inc n)
)
(define (f x) x)
(define (inc x) (+ x 1))

computing π：
(define (f n)
  (/ (* 4 n (+ n 1))
     (square (+ 1 (* n 2)))
  )
)
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (pi n)
  (exact->inexact(* 4 (product f 1 inc n)))
)

Exercise 1.32
iterative accumulate:
(define (accumulate combiner null-value term a next b)
  (
    (define (iter a result)
      (if (> a b)
        result
        (iter (next a) (combiner ( result (term a))))
      )
    )
    (iter a null-value)
  )
)
// define product procedure
(define (product term a next b)
  (accumulate * 1 term a next b)
)
recuisive accumulate:
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b ))
  )
)

Exercise 1.33
recuisive filtered accumulate:
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
    null-value
    (combiner 
      (filter (term a)) 
      (filter (accumulate combiner null-value term (next a) next b filter))
    )
  )
)
iterative filtered accumulate:
(define (filtered-accumulate combiner null-value term a next b filter)
  (
    (define (iter a result)
      (if (> a b)
        result
        (iter 
          (next a) 
          (combiner
            (filter (result)) 
            (filter (term a))
          )
        )
      )
    )
    (iter a null-value)
  )
)

Exercise 1.35
(define tolerance 0.000000000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  ) 
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next)
      )
    )
  )
  (try first-guess)
)
(define (goldratio)
  (exact->inexact(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))
)

Exercise 1.36
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  ) 
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? guess next)
        next)
        (else
          (newline)
          (display next)
          (try next)
        )
      )
    )
  )
  (try first-guess)
)

(define (nthroot)
  (exact->inexact(fixed-point (lambda (x) (/ (log 1000) (log x))) 2))
)

Exercise 1.37
// get the continued fraction value of k by iterative.
// (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)

(define (cont-frac N D k)
  (define (iter result i)
    (if (= i 0)
      result
      (iter (/ (N i) (+ (D i) result)) (- i 1))
    )
  )
  (iter (/ (N k) (D k) ) (- k 1) )          
)
recursive:
(define (cont-frac N D k)
(define (cf  i)
  (if (= i k)
    (/ (N i) (D i))
    (/ (N i) (+ (D i) (cf (+ i 1))) )
  )
)
(cf 1)     
)

(define (iter result i)
  (if (= i 0)
    result
    (iter (/ (N i) (+ (D i) result)) (- i 1))
  )
)
(iter (/ (N k) (D k) ) (- k 1) )          
)
// the continued fraction is decrement, guess the k from 1.
(define (getappk)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  ) 
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? (/ 1 (phi)) next)
        next)
        (else
          (newline)
          (display next)
          (try (+ 1 guss))
        )
      )
    )
  )
  (try 1) 
)
// below functioins get the golden ratio with tolerance 0.00001
(define (phi) (/ (+ 1 (sqrt 5) 2)))
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
1))
(define (average x y)
  (/ (+ x y) 2)
)

Exercise 1.38:
(define (D i)
  (if (= (remainder (- i 2) 3) 0)
    (+ 2.0 (* 2 (/ (- i 2) 3)))
    1.0
  )
)
(define (cont-frac N D k)
  (define (iter result i)
    (if (= i 0)
      result
      (iter (/ (N i) (+ (D i) result)) (- i 1))
    )
  )
  (iter (/ (N k) (D k) ) (- k 1) )          
)
(define (getE) 
  ( + 2.0 (cont-frac (lambda (i) 1.0) D 1000))  
)

Exercise 1.39:
(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
      x
      (* (- x) x)
    )
  )
  (define (D i)
    (- (* 2 i) 1)
  )
  (cont-frac N D k)
)
(define (cont-frac N D k)
  (define (iter result i)
    (if (= i 0)
      result
      (iter (/ (N i) (+ (D i) result)) (- i 1))
    )
  )
  (iter (/ (N k) (D k) ) (- k 1) )          
)


Exercise 1.40:
// (newtons-method (cubic a b c) 1)
(newtons-method (cubic 1 1 1) 1)
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess)
)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x))))
)
(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
)
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c))
)
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  ) 
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? guess next)
        next)
        (else
          (newline)
          (display next)
          (try next)
        )
      )
    )
  )
  (try first-guess)
)

Exercise 1.41:
(define (inc a)
  (+ a 1)
)
(define (double f)
  (lambda (x) (f (f x)) )
)
((double inc) 1)
( ((double double) inc) 5)

Exercise 1.42:
(define (compose f g)
  (lambda (x) (f (g x)) )
)
(define (square x) (* x x))
(define (inc a)
  (+ a 1)
)
((compose square inc) 6)

Exercise 1.43:
(define (repeated f n)
  (define (itera i result) 
    ( if (= i n)
      result
      (itera (+ i 1) (compose f result))
    )
  )
  (itera 1 f)
)
(define (compose f g)
  (lambda (x) (f (g x)) )
)
(define (square x) (* x x))

((repeated square 2) 5)

Exercise 1.44:
(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3))
)
(define (n-fold-smoothed f n)
  ((repeated smooth n) f)
)
Exercise 1.45:
(define (average-damp f)
  (lambda (x) (average x (f x)))
)
(define (average x y)
  (/ (+ x y) 2)
)
(define (cubert x)
  (define (func x) (lambda (y) (/ x (cube y))))
  (define (n-fold-average-damp n f) 
    ((repeated average-damp n) f)
  )
  (fixed-point (n-fold-average-damp 3 (func x))
1.0))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )
  (define tolerance 0.00001)
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? guess next)
        next)
        (else
          (newline)
          (display next)
          (try next)
        )
      )
    )
  )
  (try first-guess)
)
(define (cube x) (* x x x x x x x))
(define (repeated f n)
  (define (itera i result) 
    ( if (= i n)
      result
      (itera (+ i 1) (compose f result))
    )
  )
  (itera 1 f)
)
(define (compose f g)
  (lambda (x) (f (g x)) )
)
// 4th  => fold twice average-damp
// 5th  => fold twice average-damp
// 6th  => fold twice average-damp
// 7th  => fold twice average-damp 128
// 8th  => fold 3th average-damp 256
 y →x/y^n-1

 Exercise 2.1
 (define (gcd a b)
  (if (= b 0)
  a 
  (gcd b (remainder a b))))
 (define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
 (define (numer x) (car x))
 (define (denom x) (cdr x))
 (define (abs x)
  (cond ((> x 0) x)
  ((= x 0) 0)
  ((< x 0) (- x))))
 

 (define (make-rat n d) 
  (let ((g (gcd n d)))
  (cond ((> (* n d) 0) (cons (abs(/ n g) ) (abs (/ d g))))
        ((< (* n d) 0) (cons (* -1 (/ n g)) (abs (/ d g)))
  ))
  ))
(define one-half (make-rat -1 -2))
(print-rat one-half)

Exercise 2.2:

(define (make-segment startP endP)
  (cons startP endP))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (midpoint-segment segment)
  (make-point 
    (average (x-point (start-segment segment)) (x-point (end-segment segment))) 
    (average (y-point (start-segment segment)) (y-point (end-segment segment)))
))
(define (length-segment segment)
(sqrt (+ (square (- (x-point (start-segment segment)) (x-point (end-segment segment)))) (square (- (y-point (start-segment segment)) (- (y-point (end-segment segment))))))))
(define (average x y)
(/ (+ x y) 2))

// test
(define startp (make-point 2 2))
(define endp (make-point 4 4))
(define line1 (make-segment startp endp))
(print-point (midpoint-segment line1))

Exercise 2.3:
(define (make-rect width height)
(cons width height))
(define (width-rect rect)
(car rect))
(define (heigth-rect rect)
(cdr rect))
(define (perimeter-rect rect)
(* 2 (+ (length-segment (width-rect rect)) (length-segment (heigth-rect rect)))))
(define (area-rect rect)
(* (length-segment (width-rect rect)) (length-segment (heigth-rect rect))))


Exercise 2.5:
(define (log3 x) (/ (log x) (log 3)))
(define (log2 x) (/ (log x) (log 2)))
(define (cons a b) (* (expt 2 a) (expt 3 b)))
(define (car x) (log2 (reduce x 3)))
(define (cdr x) (log3 (reduce x 2)))
(define (reduce n x)
  (define (iter n) 
    (cond ((= 1 n) n)
      ((< 0 (remainder n x)) n)
      (else (iter (/ n x))))
  )
  (iter n)
)
//test
(define n (cons 2 3))
(car n)
(cdr n)

Exercise 2.7:
(define (lower-bound interval)
(car interval))
(define (upper-bound interval)
(cdr interval))

Exercise 2.8:
(define (sub-interval x y)
(make-interval 
  (- (lower-bound x) (upper-bound y))
  (- (upper-bound x) (lower-bound))))

Exercise 2.10:

(define (div-interval x y)
(if (or (0 = (upper-bound y)) (0 = (lower-bound y)))
  (error "zero is not allowed in y" (upper-bound y) (lower-bound y))
(mul-interval
  x
  (make-interval (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y))))))

Exercise 2.11:
(define (mul-interval x y)
(let ((p1 (* (lower-bound x) (lower-bound y)))
     (p2 (* (lower-bound x) (upper-bound y)))
     (p3 (* (upper-bound x) (lower-bound y)))
     (p4 (* (upper-bound x) (upper-bound y))))
)

Exercise 2.12:
(define (make-center-percent c p)
(make-interval (- c (* c p)) (+ c (* c p))))
(define (percent i)
(/ (width i) (center i)))
(define (center i)
(/ (+ (lower-bound i) (upper-bound i)) 2))

Exercise 2.17:

(define (last-pair list)
  (define (iter list)
    (if (null? (cdr list))
      (car list)
      (iter (cdr list))
    )
  )
  (iter list)
)

Exercise 2.18:
(define (reverse list1)
  (define (iter listx result)
    (if (null? listx)
      result
      (iter (cdr listx) (cons (car listx) result))
    )
  )
  (iter list1 (list ))
)

Exercise 2.19:
(define (except-first-denomination coin-values)
  (cdr coin-values)
)
(define (first-denomination coin-values)
  (car coin-values)
)
(define (no-more? coin-values)
  (null? coin-values)
)

Exercise 2.20:
(define (same-parity . listx)
  (define (iter a)
    
    (cond ((null? a) (list ))
          ((= (remainder (car listx) 2) (remainder (car a) 2)) (cons (car a) (iter (cdr a))))
          (else (iter (cdr a)))
    )
  )
  (iter listx)
)
Exercise 2.21:
(define (square-list items)
  (if (null? items)
  (list )
  (cons (square (car items)) (square-list (cdr items))))) 

(define (square-list items)
(map square items))
(define (map proc items)
    (if (null? items)
        (list )
        (cons (proc (car items)) (map proc (cdr items)))
    )
)

Exercise 2.23:
(define (for-each proc items)
    (cond 
      (
        (not(null? items)) 
        (proc (car items))
        (for-each proc (cdr items))
      )
    
    )
)
(for-each (lambda (x) (newline)
  (display x))
  (list 57 321 88))

Exercise 2.25:
(define (count-leaves x) 
  (cond (
    (null? x) 0)
    ((and(not (pair? x)) (= x 7)) (display x))
    (else (count-leaves (car x))
          (count-leaves (cdr x))
    )
  )
)

(count-leaves (1 3 (5 7) 9))


Exercise 2.27:
(define (deep-reverse x)
(define a (reverse x))
(map (lambda (x) (
  cond ((pair? x) (reverse x))
        (else  x)
)) a)
)
; test:
; (define a (list 1 2 (list 3 4 5)))
; (define b (list 1 2))
; (deep-reverse a)
; (deep-reverse b)

Exercise 2.28:
(define (fringe x)
  (cond ((null? x) ())
    ((not (pair? x)) (list x))
    (else (append () (fringe (car x))
              (fringe (cdr x))))))
; test:
; (define a (list 1 (list 1 2) 3 (list 4 5 6) 7))

Exercise 2.29:
(define (left-branch mobile)
  (car mobile)
)
(define (right-branch mobile)
  (cadr mobile)
)
(define (branch-length branch)
  (car branch)
)
(define (branch-structure branch)
  (cadr branch)
)
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))
          ))
)
(define (blanced? mobile)
  (= (* (total-weight (left-branch mobile)) (branch-length (left-branch mobile)))
     (* (total-weight (right-branch mobile)) (branch-length (right-branch mobile)))))

)

Exercise 2.30:
using map:
(define (square-tree tree ) 
  (map 
    (lambda (sub-tree) (
      if (pair? sub-tree)
        (square-tree sub-tree)
        (square sub-tree))
    )
    tree
  )
)

using recursive:
(define (square-tree tree ) 
  (cond ((null? tree)      (list ))
        ((not(pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))
  )
)
Exercise 2.31:

(define (tree-map proc tree)
  (define (iter tree) 
    (map 
      (lambda (sub-tree) (
        if (pair? sub-tree)
          (iter sub-tree)
          (proc sub-tree))
      )
      tree
    )
  )
  (iter tree)
)

Exercise 2.32:
(lambda (x)
  (append  (list (car s)) x)
)

(define (subsets s) 
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
            (append  (list (car s)) x)
          ) rest)
        ))
  )
)
; (subsets (list 1 2 3))

Exercise 2.33:
(define (map p sequence)
(accumulate (lambda (x y) (cons (p x) y)) nil sequence))


(define (append seq1 seq2) (accumulate cons seq2 seq1))

(define (length sequence) (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

Exercise 2.34:

(define (horner-eval x coefficient-sequence)
  (accumulate 
    (lambda (this-coeff higher-terms)
      (+ this-coeff (* x higher-terms))
    )
    0
    coefficient-sequence
  )
)

Exercise 2.35:
(define (count-leaves t)
  (accumulate
    +
    0
    (map
      (lambda (x)
        (cond ((pair? x) (count-leaves x))
              (else 1)
        )
      )
      t
    )
  )
)

Exercise 2.36:
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons
        (accumulate op init (map (lambda (x) (car x)) seqs))
        (accumulate-n op init (map (lambda (x) (cdr x)) seqs))
      )
  )
)

Exercise 2.37:
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m)
)
(define (transpose mat)
  (accumulate-n cons nil mat)
)

(define (matrix-*-matrix m n) 
  (
    let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)
  )
)
; test:
(define a (list (list 1 2) (list 3 4)))
(define c (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define v (list 1 2))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(matrix-*-vector a v)

Exercise 2.39:
(define (reverse sequence)
(fold-right 
  (lambda (x y) 
    (cons y x)
  ) 
  nil sequence))


(define (reverse sequence)
(fold-left (lambda (x y) (cons y x)) nil sequence))

Exercise 2.40:
(define (unique-pairs n)
  (accumulate append nil 
    (map (lambda(i) (map (lambda (j) (list j i)) (enumerate-interval 1 (- i 1) ))) (enumerate-interval 1 n))
  )
)

Exercise 2.41:
(define (triples n)
  (accumulate append nil
  (map (lambda (x)
        (accumulate append nil 
         (map (lambda (y) (
                map (lambda (z) 
                  (list z y x)
              ) (enumerate-interval 1 (- y 1))
          )) (enumerate-interval 1 (- x 1))
         )
        )
       )
       (enumerate-interval 1 n)
  )
  )
)

(define (func241 n s)
  (accumulate append nil
    (map (lambda (i)
        (if (= (sum i) s)
          i
          nil
        )
      )
      (triples n)
    )  
  )    
)
(define (sum items)
    (accumulate + 0 items)
)

Exercise 2.42:
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board) 
      (filter 
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens) ;((1 1))
            (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens)
                  )
                  (enumerate-interval 1 board-size)
            )
          )
          (queen-cols (- k 1)) ;  (((1 1)))
        )
      )
    )
  )
  (queen-cols board-size)
)

(define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list(list k new-row)) )
)
(define empty-board nil)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
          (cons (car sequence)
          (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (safe? k positions)
  (if (null? positions) #t
    (and (safe-p-ps? (car positions) (cdr positions))
        (safe? k (cdr positions))
    )
  )
)
  ; (
  ;   ((1 1) (2 1))
  ;   ((1 1) (2 2))
  ;   ((1 2) (2 1))
  ;   ((1 2) (2 2))
  ; )
(define (safe-2p? positionA positionB)
  (define x1 (car positionA))
  (define y1 (cadr positionA))
  (define x2 (car positionB))
  (define y2 (cadr positionb))
  (not(or (= x1 x2)
          (= y1 y2)
          (= (abs(- x1 x2)) (abs(- y1 y2)))
  ))
)
(define (safe-p-ps? position positions)
  (if (null? positions) #t
      (if (safe-2p? position (car positions))
          (safe-p-ps? position (cdr positions))
          #f
      )
  )
)
;
(define x (list (list 1 1) (list 2 3)))
(define p (list 1 2))

Exercise 2.43:
reason: (queen-cols (- k 1)) excuted in every (enumerate-interval 1 board-size) iterative

Exercise 2.44:
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
    painter
    (let (
          (up (up-split painter (- n 1)))
          (right (right-split painter (- n 1)))
         )
      (let (
            (top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1)))
          )
        (beside (below painter top-left)
                (below bottom-right corner))
      )
    )
  )
)


(define (frame-coord-map frame) 
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))
      )
    )
  )
)


Exercise 2.46:
(define (make-vect x y) (list x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))
(define (add-vect v1 v2)
  (list (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)))
)
(define (sub-vect v1 v2)
  (list (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)))
)
(define (scale-vect s v)
  (list (* s (xcor-vect v)) (* s (ycor-vect v)))
)

//frame selector
(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame frame)
      (car frame))
(define (edge1-frame frame)
      (cadr frame)
)
(define (edge2-frame frame)
  (caddr frame)
)


(define (segments->painter segment-list) (lambda (frame)
(for-each
(lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment)))
)
     segment-list))
)


Exercise 2.48:
(define (make-segment v1 v2) (list v1 v2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cadr segment))

Exercise 2.49:
a:
(
  ((0 0) (0 1))
  ((0 1) (1 1))
  ((1 1) (1 0))
  ((1 0) (0 0))
)
b:
(
  ((0 0) (0.5 0.5))
  ((0.5 0.5) (0 1))
  ((0 1) (0.5 0.5))
  ((0.5 0.5) (1 1))
  ((1 1) (0.5 0.5))
  ((0.5 0.5) (1 0))
)

Exercise 2.50:
(define (flip-horiz painter)
  (transform-painter painter
      (make-vect 1.0 0.0) ; new origin
      (make-vect 0.0 0.0) ; new end of edge1
      (make-vect 1.0 1.0))) ; new end of edge2

Exercise 2.51:
plan A:
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below (transform-painter
                painter1
                (make-vect 0.0 0.0)
                (make-vect 0.0 1.0)
                split-point))
              (paint-top
              (transform-painter
                painter2
                split-point
                (make-vect 1.0 0.0)
                (make-vect 0.0 1.0))))
(lambda (frame) (paint-below frame) (paint-top frame)))))

plan B:
(define (below painter1 painter2)
   (let ((new-painter (beside (clockwise90 painter1) (clockwise90 painter2))))
      (let ((rotated-painter (rotate90 new-painter)))
        (lambda (frame) (rotated-painter frame))
      )          
   )             
)

(define (clockwise90 painter) 
  (transform-painter painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

Exercise 2.54:

(define (equal? list1 list2)
    (cond ((and (null? list1) (null? list2)) #t)
          ((or (null? list1) (null? list2)) #f)
          (else 
            (and (eq? (car list1) (car list2)) (equal? (cdr list1) (cdr list2))))
    )
)

Exercise 2.56:
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base s) (cadr x))
(define (exponent s) (caddr x))
(define (make-exponentiation base exponent) 
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))
  )
)
(define (=number? a num) (and (number? a) (= num a)))

Exercise 2.57:
(define (augend p)
  (accumulate make-sum 0 (cddr p))
)
(define (multiplicand p)
(accumulate make-product 1 (cddr p))
)
(define (accumulate op initial sequence)
  (if (null? sequence)
  initial
  (op (car sequence)
  (accumulate op initial (cdr sequence)))))


Exercise 2.58:
a:
(x + (3 * (x + (y + 2))))

1. change sum procedure:
(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2))
    (+ a1 a2))
    (else (list a1 '+ a2))))

(define (addend s) (car s))
(define (augend s) (caddr s))
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
2.change multiple procedure:
(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list m1 '* m2))))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

Exercise 2.59:

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

Exercise 2.61:
(define (addjoin-set x set)
  (cond ((null? set) (cons x set))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (addjoin-set x (cdr set))))
  )
)

Exercise 2.68:

(define (exist-in x items) ;only for symbols
  (cond ((null? items) #f)
        ((eq? x (car items)) #t) 
        (else (exist-in x (cdr items)))
  )
)

(define (encode-symbol letter tree)
  (cond ((and (leaf? tree) (eq? (symbol-leaf tree) letter)) '()) 
        ((exist-in letter (symbols (left-branch tree))) (append '(0) (encode-symbol letter (left-branch tree)) ))
        ((exist-in letter (symbols (right-branch tree))) (append '(1) (encode-symbol letter (right-branch tree)) ))
        (else (error "bad letter" letter))
  )
)

Exercise 2.69:
(define (successive-merge leaf-pairs)
  (define (a leaf-pairs)
    (cond ((null? (cdr leaf-pairs)) leaf-pairs)
          (else
            (a
              (adjoin-set
                (make-code-tree (car leaf-pairs) (cadr leaf-pairs))
                (cddr leaf-pairs)
              )
            )
          )
    )
  )
   (car (a leaf-pairs))               
)
(((leaf a 1) ((leaf b 1) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8))

Exercise 2.70:
(define pairs2 '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define tree2 (generate-huffman-tree pairs2))

Exercise 2.71:
(define pairs271 '((a 1) (b 2) (c 4) (d 8) (e 16)))
(define tree271 (generate-huffman-tree pairs271))


(define (real-part x))
(define (imag-part x))
(define (magnitude x))
(define (angle x))
(define (make-from-real-imag ))
(define (make-from-mag-ang ))


(define (install-drive)
    (define (sum operands var) 
      (make-sum (deriv (addend operands) var)
                (deriv (augend operands) var)
      )
    )
    (define (product operands var)
      (make-sum (make-product
                  (multiplier operands)
                  (deriv (multiplicand operands) var))
                (make-product
                  (deriv (multiplier operands) var)
                  (multiplicand operands)))
    )
    (put 'deriv '(+) sum)
    (put 'deriv '(*) product)
)

Exercise 2.75
(define (make-from-mag-ang r a)
  (lambda (op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitute) r)
          ((eq? op 'angle) a)
          (else (error "unknow op:make-from-mag-ang" op))
    )
  )
)
;test
(define z (make-from-mag-ang 1 0.785398))
(z 'angle)

Exercise 2.83:
(define (integer->rational n)
  (make-rational n 1)
)
(define (rational->real n)
  (make-scheme-number n)
)
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0)
)
(put-coercion 'raise
              'integer
              integer->rational
)
(put-coercion 'raise
              'rational
              rational->real
)
(put-coercion 'raise
              'scheme-number
              scheme-number->complex
)
(define types-tower (list 'integer 'rational 'scheme-number 'complex))
(define (height type1 )
   (index type1 types-tower)
)
(define (successive-rais arg1 arg2)
  (let ( (type1 (type-tag arg1))
         (type2 (type-tag arg2))
       )
    (cond ((= (height type1) (height type2))
            (list arg1  arg2))
          ((> (height type1) (height type2))
            (successive-rais arg1 ((get-coercion 'raise type2) arg2))
          ((< (height type1) (height type2))
          (successive-rais ((get 'raise type1) arg1) type2))
    )
  )
)
(define (index x items)
  (define (recuisive x items result)
    (if (eq? x (car items))
        result
        (recuisive x (cdr items) (+ result 1))
    )
  )
  (recuisive x items 0)
 )
Exercise 2.84:
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (if (equal? type1 type2)                                    ; 新增
                            (error "No method for these types" (list op type-tags)) ; 
                            (let ((same-type-args ((successive-rais a1 a2)))
                                  (a1 (car same-type-args))
                                  (a2 (cadr same-type-args)))
                                  (apply-generic op a1 a2)
                                )))
                    (error "No method for these types"
                            (list op type-tags)))))))

Exercise 2.85:
(put-samplify 'drop
            'complex
            drop-complex
)
(define (drop-comlex n)
  (cond ((= (imag-part n) 0) (make-rational (real-part n) 1))
        (else #f)
  )
)
(put-samplify 'drop
              'real
              drop-real
)
(define (drop-real n)

)
(put-samplify 'drop
              'rational
              drop-rational
)
(define (drop-rational n)
  (cond ((= n (round n)) (make-scheme-number n))
        (else #f)
  
  )
)
(put-samplify 'drop
              'scheme-number
              drop-scheme-number
)
(define (drop-scheme-number n)
  #f
)
(define (drop x)
  (let (
        (lower ((get 'drop (type-tag x)) x))
      )
      if(lower
        (drop lower)
        x
      )
  )
)

Exercise 2.87:
(define (zero? x)
    ((get 'zero? (type-tag x)) x)
)
(define (install-zero?-number)
  (define (tag x) (attach-tag 'number x))
  (put 'zero? 'number
        (tag (lambda (x) (= 0 x)))
  )
)
(define (install-zero?-rational)
  (define (tag x) (attach-tag 'rational x))
  (put 'zero? 'rational
        (tag (lambda (x) (= 0 x)))
  )
)
(define (install-zero?-real)
  (define (tag x) (attach-tag 'real x))
  (put 'zero? 'real
        (tag (lambda (x) (= 0 x)))
  )
)
(define (install-zero?-complex)
  (define (tag x) (attach-tag 'complex x))
  (put 'zero? 'complex
        (tag (lambda (x) (and (= 0 (real-part x)) (0 = (imag-part x)))))
  )
)
(define (install-zero?-polynomial)
  (define (tag x) (attach-tag 'polynomial x))
  (put 'zero? 'polynomial
        (tag (lambda (x) (null? x)))
  )
)

Exercise 2.88:
(define (negation x)
    ((get 'negation (type-tag x)) x)
)
(define (install-negation-number)
  (define (tag x) (attach-tag 'number x))
  (put 'negation 'number
        (tag (lambda (x) -x))
  )
)
(define (install-negation-rational)
  (define (tag x) (attach-tag 'rational x))
  (put 'negation 'rational
        (tag (lambda (x) -x))
  )
)
(define (install-negation-real)
  (define (tag x) (attach-tag 'real x))
  (put 'negation 'real
        (tag (lambda (x) -x))
  )
)
(define (install-negation-complex)
  (define (tag x) (attach-tag 'complex x))
  (put 'negation 'complex
        (tag (lambda (x)
        (make-complex-from-real-imag 
          (negation (real-part x))
          (negation (imag-part x)))))
  )
)
(define (install-negation-polynomial)
  (define (tag x) (attach-tag 'polynomial x))
  (put 'negation 'polynomial
        (tag (lambda (x)
          (let ((terms (term-list x)))
               (make-polynomial (variable x) (map negation terms))
          )
        ))
  )
)
(define (sub-poly p1 p2)
    (add-poly p1 (negation p2))
)

Exercise 2.89:

(define (intall-polynomial-dense)
  (define (adjoin-term term term-list)
      (if (=zero? (coeff term))
          term-list
          (cons term term-list)
      )
  )
  (define (make-term order coeff)
      coeff
  )
  (define (order term)
      (length (cdr term-list))
  )
  (define (coeff term)
      term
  )
  (define (tag x) (attach-tag 'dense x))
  (put 'adjoin-term
       'dense
       adjoin-term
  )
  (put 'make-term
       'dense
       make-term
  )
  (put 'order
       'dense
       order
  )
  (put  'coeff
        'dense
        coeff
  )
)

Exercise 2.90:
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term L1))
          (t2 (first-term L2)))
      (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (- (order t1) (order t2))))
              (let ((new-term (make-term new-o new-c)))
                  (let ((rest-of-result (div-terms (sub-terms L1 (mul-term-by-all-terms new-term L2)) L2)))
                      (list (adjoin-term new-term (car rest-of-result)) (cadr rest-of-result))
                  )
              )
          )
      )
    )
  )
)

(define term-list1 (list (list 5 1) (list 0 -1)))
(define term-list2 (list (list 2 1) (list 0 -1)))
(define term-list3 (list (list 1 1) (list 0 1)))
~/wangshaosen/codes/demo

