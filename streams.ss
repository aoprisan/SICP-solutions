(define (delay expr) (lambda() (expr)))

(define (force delayed) (delayed))

(define (cons-stream x y)
  (cons x (delay y)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
	 (stream-cons (stream-car s)
		      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s))))) 
		       
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr) (- n 1))))

(define (stream-null? s) (false))

(define the-empty-stream '())

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))


(define str (cons-stream 1 2))

(define (stream-enumerate-interval low hight)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
