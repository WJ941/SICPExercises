(define (iter-create-table keys value)
  (if (null? (cdr keys))
        (cons (car keys) value)
        (cons (car keys)
              (cons (iter-create-table (cdr keys) value) '())
        )              
      
  )
)
(define (lookup-recursive keys table)
  (let ((record (assoc (car keys) (cdr table))))
    (if record
    (cdr record)
  false)))


(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records)) ;replace equal? with same-key?
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup  keys)
      (let ((subtable  (lookup-recursive keys local-table)))
        (if subtable
          (lookup-recursive (cdr keys) subtable)
          false)))

    (define (insert! keys value)
      (let ((subtable (assoc (car keys) (cdr local-table))))
        (if subtable
          (set-cdr! subtable
                    (cons (iter-create-table (cdr keys) value)
                          (cdr subtable)))
          (set-cdr! local-table
                    (cons (iter-create-table (cdr keys) value)
                          (cdr local-table))))
    'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
  dispatch))

(define (same-key? x y)
  (cond ((symbol? x) (lambda (x y) (eq? x y)))
        ((number? x) (lambda (x y) (= x y)))
  )
)