(define (filter predicate seq)
  (cond ((null? seq) (list))
        ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))
  )
)

(define (accumulate op initial seq)
 (if (null? seq) 
     initial
     (op (car seq) (accumulate op initial (cdr seq)))
 )
)


(define (enum-int low high)
 (if (> low high)
     (list)
     (cons low (enum-int (+ 1 low) high))
 )
) 

(define (enum-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enum-tree (car tree)) (enum-tree (cdr tree)) ))
  )
)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2)) ) )
  )
)

(define (even-fibs n)
  (accumulate cons (list) (filter even? (map fib (enum-int 0 n))))
)

;;ex 2.33
(define (mapc p sequence)
  (accumulate (lambda (x y) 
     (cons (p x) y)) 
    (list) sequence)
)

(define (appendc seq1 seq2)
  (accumulate cons seq2 seq1))


(define (lengthc sequence)
  (accumulate (lambda (x y) (+ 1 y))
 0 sequence))

;;ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x )) )
              0
              coefficient-sequence)
)



(define (x) (horner-eval 2 (list 1 3 0 5 0 1)) )


;ex 2.35
(define tr (list (list 1 2) (list 3 4)))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (not (pair? x))
                                         1
                                         (count-leaves x)  
                                     )
                         )
  t))
)
;ex 2.36
(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)) )
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs) )
            (accumulate-n op init (map cdr seqs) ))
  )
)

;ex 2.37
(define mat (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)) )
(define vec1 (list 1 2 3 4) )
(define vec2 (list 4 5 6 6) )

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi) (matrix-*-vector n mi )) m)))
