; The cartesian product of two lists.
(library (product)
  (export product)
  (import (chezscheme))
  (define (product l0 l1)
    (let loop ([a l0] [b l1] [va '()] [vb '()])
      (cond
        [(null? b) (values va vb)]
        [(null? a) (loop l0 (cdr b) va vb)]
        [else (loop (cdr a) b (cons (car a) va) (cons (car b) vb))]))))
