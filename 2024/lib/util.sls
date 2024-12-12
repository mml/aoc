(library (util)
  (export id get-lines-from-file gridvector-from-file maybe-enqueue!)
  (import (chezscheme)
          (gridvector))
  (define (id x) x)
  (define (get-lines-from-file path)
    (with-input-from-file path
      (lambda ()
        (let loop ([line (get-line (current-input-port))][lines '()])
          (cond
            [(eof-object? line) (reverse! lines)]
            [else (loop (get-line (current-input-port)) (cons line lines))])))))
  (define gridvector-from-file
    (case-lambda
      [(path f)
       (let ([lines (get-lines-from-file path)])
         (let* ([w (string-length (car lines))]
                [h (length lines)]
                [g (make-gv w h)])
           (let loop ([y 0][lines lines])
             (cond
               [(null? lines) g]
               [else
                 (for-each
                   (lambda (x char)
                     (gv-set! g x y (f char)))
                   (iota w)
                   (string->list (car lines)))
                 (loop (add1 y) (cdr lines))]))))]
      [(path)
       (gridvector-from-file path id)]))

  ; Work queues
  (define (maybe-enqueue! q x)
    (if (null? q)
      (list x)
      (let loop ([pair q][hd (car q)][tl (cdr q)])
        (cond
          [(equal? hd x) q]
          [(null? tl)
           (set-cdr! pair (list x))
           q]
          [else
            (loop tl (car tl) (cdr tl))]))))

  );
