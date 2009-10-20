(define l1 (list 1 3 (list 5 7) 9))

(define l2 (list (list 7)))

(define l3 (cons 1 (cons 2 (cons 3 ( cons 4 ( cons 5 (cons 6 7)))))))

(define m 
 (cdr (cdr (cdr (cdr (cdr (cdr l3))))))
)

(define x1 (list 1 2 3))
(define y1 (list 4 5 6))

(define x2 (list (list 1 2) (list 3 4)))


(define (deep-reverse l)
 (define (rev l result) 
   (cond ((null? l) result)
         ((not (pair? l)) l)
         (else (rev (cdr l) (cons (rev (car l) (list)) result ) ))
        )
   )
 (rev l (list))
)

(define x3 (list 1 2 3 4))
(define x4 (list (list 1 2 3 4) (list 5 6 7 8)))
(define x5 (list x4 x4))

(
define (fringe t)
 (cond ((null? t) (list))
          ((not (pair? t)) (list t))
          (else (append (fringe (car t)) (fringe (cdr t))))      
 )
)

(define (fringe1 t)
 (define (f1-iter ls result)
  (cond ((null? ls) result)
        ((not (pair? ls)) (cons ls result))
        (else (f1-iter (car ls) (f1-iter (cdr ls) result)))      
  )
 )     
 (f1-iter t (list))      
)


(define (make-mobile left right)
  (list left right)
)

(define (make-branch length structure)
  (list length structure)
)

(define (left-branch mobile)
  (car mobile)
)

(define (right-branch mobile)
  (cdr mobile)
)

(define (branch-length branch)
 (car branch)
)

(define (branch-structure branch)
  (cdr branch)
)
  
  
