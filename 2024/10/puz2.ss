#!/usr/bin/env -S scheme --program
(import (chezscheme) (gridvector) (product))
(define (numeral->number c)
  (assert (char-numeric? c))
  (- (char->integer c) (char->integer #\0)))
(define (newline? c)
  (eq? #\newline c))
(define (read-file f)
  (with-input-from-file f
    (lambda ()
      (let loop ([row '()][rows '()][c (read-char)])
        (cond
          [(eof-object? c) (reverse rows)]
          [(newline? c) (loop '() (cons (reverse row) rows) (read-char))]
          [(char-numeric? c) (loop (cons (numeral->number c) row) rows (read-char))]
          [else (error 'read-file "unexpected character" c)])))))
(define (rows->gv rows)
  (let ([w (length (car rows))] [h (length rows)])
    (let ([gv (make-gv w h)])
      (let outer ([y 0] [rows rows])
        (if (null? rows)
          gv
          (let inner ([x 0] [row (car rows)])
            (cond
              [(null? row)
               (outer (add1 y) (cdr rows))]
              [else
                (gv-set! gv x y (car row))
                (inner (add1 x) (cdr row))])))))))
(define (<==> lo x hi)
  (and (<= lo x) (<= x hi)))
(define (list-fill x n)
  (if (zero? n)
    '()
    (cons x (list-fill x (sub1 n)))))
(define (neighbors gv x y)
  (let ([xmax (sub1 (gv-width gv))]
        [ymax (sub1 (gv-height gv))])
    (let ([xs (filter (lambda (n) (<==> 0 n xmax)) (list (sub1 x) (add1 x)))]
          [ys (filter (lambda (n) (<==> 0 n ymax)) (list (sub1 y) (add1 y)))])
      (values (append xs (list-fill x (length ys)))
              (append (list-fill y (length xs)) ys)))))
(define (score-path gv x y n)
  (define visited (make-gv (gv-width gv) (gv-height gv)))
  (define (help x y n)
    (let ([v (gv-ref gv x y)])
      (cond
        [(gv-ref visited x y) 0]
        [else
          (cond
            [(not (= n v)) 0]
            [(= 9 v)
             1]
            [else
              (call-with-values (lambda () (neighbors gv x y))
                                (lambda (xs ys)
                                  (apply + (map (lambda (x y) (help x y (add1 n))) xs ys))))])])))
  (help x y n))

(define (main f)
  (let* ([rows (read-file f)]
         [gv (rows->gv rows)])
    (let-values ([(xs ys) (product (iota (gv-width gv))
                                   (iota (gv-height gv)))])
      (printf "~a~%"
              (apply + (map (lambda (x y)
                              (score-path gv x y 0)) xs ys))))))
(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
