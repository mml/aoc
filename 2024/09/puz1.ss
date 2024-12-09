#!/usr/bin/env -S scheme --program
(import (chezscheme))

(define (newline? c)
  (eq? #\newline c))
(define (char->len c)
  (- (char->integer c) (char->integer #\0)))
(define (read-file fname)
  (with-input-from-file fname
    (lambda ()
      (let loop ([blocks 0][file-lens '()][free-lens '()])
        (let ([fic (read-char)])
          (if (or (eof-object? fic) (newline? fic))
            (values blocks (reverse! file-lens) (reverse! free-lens))
            (let ([file-len (char->len fic)][frc (read-char)])
              (if (or (eof-object? frc) (newline? frc))
                (loop (+ blocks file-len) (cons file-len file-lens) free-lens)
                (let ([free-len (char->len frc)])
                  (loop (+ blocks file-len free-len)
                        (cons file-len file-lens)
                        (cons free-len free-lens)))))))))))

(define (make-block-vector fname)
  (let-values ([(blocks file-lens free-lens) (read-file fname)])
    (let ([v (make-vector blocks #f)])
      (let loop ([id 0] [n 0] [file-lens file-lens] [free-lens free-lens])
        (cond
          [(null? file-lens) v]
          [else
            (let ([file-len (car file-lens)])
              (let loop ([i file-len])
                (unless (zero? i)
                  (vector-set! v n id)
                  (set! n (add1 n))
                  (loop (sub1 i)))))
            (if (null? free-lens)
              (loop (add1 id) n (cdr file-lens) free-lens)
              (loop (add1 id) (+ n (car free-lens)) (cdr file-lens) (cdr free-lens)))])))))

(define (compact v)
  (let loop ([l 0][r (sub1 (vector-length v))])
    (cond
      [(< r l) v]
      [(not (vector-ref v r)) (loop l (sub1 r))]
      [(vector-ref v l) (loop (add1 l) r)]
      [else
        (vector-set! v l (vector-ref v r))
        (vector-set! v r #f)
        (loop (add1 l) (sub1 r))])))

(define (checksum v)
  (let loop ([n 0] [total 0])
    (if (= n (vector-length v))
      total
      (let ([x (vector-ref v n)])
        (loop (add1 n) (+ total
                          (if x
                            (* x n)
                            0)))))))

(define (main fname)
  (let ([v (make-block-vector fname)])
    (printf "~a~n" (checksum (compact v)))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
