(import (chezscheme))

(define (read-input)
  (let* ([path (car (command-line-arguments))]
         [port (open-input-file path)])
    (let loop ([l1 '()] [l2 '()])
      (let* ([d1 (get-datum port)]
             [d2 (get-datum port)])
        (cond
          [(eof-object? d1) (values l1 l2)]
          [(eof-object? d2) (error 'read-input "Incomplete line" d1)]
          [else (loop (cons d1 l1) (cons d2 l2))])))))

(define (difference a b)
  (if (> a b)
    (- a b)
    (- b a)))

(define (find-sum)
  (let-values ([(l1 l2) (read-input)])
    (let loop ([l1 (sort < l1)]
               [l2 (sort < l2)]
               [sum 0])
      (if (null? l1)
        sum
        (loop (cdr l1)
              (cdr l2)
              (+ sum (difference (car l1)
                                 (car l2))))))))

(printf "~a~n" (find-sum))
