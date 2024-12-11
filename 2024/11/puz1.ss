#!/usr/bin/env -S scheme --program
(import (chezscheme))
(define (read-file f)
  (with-input-from-file f
    (lambda ()
      (let loop ([d (read)] [l '()])
        (cond
          [(eof-object? d) (reverse l)]
          [else
            (loop (read) (cons d l))])))))
(define (blink stones)
  (if (null? stones)
    '()
    (let* ([stone (car stones)]
           [digits (number->string stone)]
           [len (string-length digits)])
      (cond
        [(zero? stone) (cons 1 (blink (cdr stones)))]
        [(even? len)
         (let* ([len/2 (/ len 2)]
                [l (string->number (substring digits 0 len/2))]
                [r (string->number (substring digits len/2 len))])
           (cons l (cons r (blink (cdr stones)))))]
        [else
          (cons (* 2024 stone) (blink (cdr stones)))]))))
(define (main f)
  (time
    (let ([stones (read-file f)])
      (let loop ([i 0][stones stones])
        (if (= i 25)
          (printf "~a~%" (length stones))
          (loop (add1 i) (blink stones)))))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
