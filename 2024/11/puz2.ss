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
(define (stones->hashtable stones)
  (let ([h (make-eq-hashtable)])
    (for-each (lambda (stone)
                (hashtable-update! h stone add1 0))
              stones)
    h))
(define (even-digits n)
  (let loop ([lo 10] [divisor 10])
    (let ([hix (fx* 10 lo)])
      (if (< n hix)
        (and (<= lo n)
             (let* ([l (fx/ n divisor)]
                    [r (fx- n (fx* l divisor))])
               (list l r)))
        (loop (fx* 100 lo) (fx* 10 divisor))))))
(define (hashtable-blink h1)
  (let* ([h2 (make-eq-hashtable)]
         [incr
           (lambda (stone n)
             (hashtable-update! h2 stone (lambda (x) (+ n x)) 0))])
    (let-values ([(ks vs) (hashtable-entries h1)])
      (vector-for-each
        (lambda (k v)
          (if (zero? k)
            (incr 1 v)
            (let ([d (even-digits k)])
              (cond
                [d
                  (incr (car d) v)
                  (incr (cadr d) v)]
                [else
                  (incr (* 2024 k) v)]))))
        ks vs))
    h2))
(define (hashtable-count h)
  (let-values ([(k v) (hashtable-entries h)])
    (apply + (vector->list v))))
(define (main f)
  (time
    (let ([stones (read-file f)])
      (let loop ([i 0][h (stones->hashtable stones)])
        #;(printf "i=~3a len=~20a~%" i (hashtable-count h))
        (if (= i 75)
          (printf "~a~%" (hashtable-count h))
          (loop (add1 i) (hashtable-blink h)))))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
