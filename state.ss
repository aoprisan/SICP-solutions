(define with-state
  (let ((x 0))
    (lambda () (set! x (+ x 1)) x)
    ))

(define (no-state)
  (let ((x 0))
    (set! x (+ x 1))
    x))

 (define no-state-ld
   (lambda ()
     (let ((x 0))
       (set! x (+ x 1))
       x)
     ))

  