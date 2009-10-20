(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items)))) 

(define nil '())

(define (rev items)
  (if (null? (cdr items))
      items
      (append (rev (cdr items)) (list (car items)))))
      ;;(cons (rev (cdr items)) (car items))))

(last-pair (list 1 2 3))
(rev (list 1 2 3))
