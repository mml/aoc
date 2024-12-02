(import (chezscheme))

(define (blank-line? line)
  (let loop ([cs (string->list line)])
    (cond
      [(null? cs) #t]
      [(char-whitespace? (car cs)) (loop (cdr cs))]
      [else #f])))

(define (line->integer line)
  (let loop ([cs (string->list line)][n #f])
    (cond
      [(null? cs) n]
      [(char-numeric? (car cs))
       (let ([m (- (char->integer (car cs)) (char->integer #\0))])
         (loop (cdr cs) (+ m (* 10 (or n 0)))))]
      [(char-whitespace? (car cs))
       (or n (loop (cdr cs) #f))]
      [else #f])))

(define (get-calories port)
  (let loop ([line (get-line port)]
             [sum 0])
    (cond
      [(eof-object? line) (list sum)]
      [(blank-line? line) (cons sum (get-calories port))]
      [else (loop (get-line port) (+ sum (line->integer line)))])))

(printf "~a~n" (apply max (call-with-input-file (car (command-line-arguments)) get-calories)))
