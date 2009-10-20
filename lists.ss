(define l1 (list 1 2 3 4))


(define (l-ref l n)
 (cond ((null? l) 0)
       ((= n 0) (car l))
       (else (l-ref (cdr l) (- n 1)))
 )
)

(define (len1 l)
 (if (null? l)
     0
     (+ 1 (len1 (cdr l)))
 )
)

(define (len2 l)
 (define (iter ls len)
   (if (null? ls)
       len
       (iter (cdr ls) (+ 1 len))
   )
 )
 (iter l 0) 
)

(define (lappend l1 l2)
 (if (null? l1)
      l2
      (cons (car l1) (lappend (cdr l1) l2))
 )
)

(define (last-pair l)
 (if (null? (cdr (cdr l)))
     l
     (last-pair (cdr l))
 )    
)


(define (rev1 l)
 (if (null? l)
     l
     (append (rev1 (cdr l)) (list (car l)))
 ) 
)

(define (rev2 l)
 (define (iter l res)
  (if (null? l)
      res
      (iter (cdr l) (cons (car l) res))
  )
 )
 (iter l (list))
)

(define (same-arrity n . ls)
 (define (find test l)
    (cond ((null? l) (list))
          ((test (car l)) (cons (car l) (find test (cdr l))))
          (else (find test (cdr l))) 
    )    
 )
 
 (if (odd? n)
     (cons n (find odd? ls))
     (cons n (find even? ls))
 )
)

(define (lmap proc ls)
 (if (null? ls)
     (list)
     (cons (proc (car ls)) (lmap proc (cdr ls)) )
 )
)




