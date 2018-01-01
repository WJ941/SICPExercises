proc a : (set! x ((s (lambda () (* x x))))))
proc b : (s (lambda () (* x x)))
proc c : (s (lambda () (set! x (+ x 1)))))
121: c->b->a 
101: b->a->c
100: b->c->a