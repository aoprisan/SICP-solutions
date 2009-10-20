(define hello-world
  (lambda ()
    (begin
      (write 'Hello)
      (newline)
      (hello-world))))