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

(define (make-counter l)
  (define h (make-eq-hashtable))
  (for-each (lambda (x)
              (eq-hashtable-update! h x add1 0))
            l)
  (lambda (k)
    (eq-hashtable-ref h k 0)))

(define (find-sum)
  (let-values ([(l1 l2) (read-input)])
    (let ([count (make-counter l2)])
    (let loop ([l1 l1] [sum 0])
      (if (null? l1)
        sum
        (let ([x (car l1)])
          (loop (cdr l1)
                (+ sum (* x (count x))))))))))

(printf "~a~n" (find-sum))
