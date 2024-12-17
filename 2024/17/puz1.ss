#!/usr/bin/env -S scheme --program
(import (chezscheme) (util) (for))
(define opcodes '((0 . adv)   ; combo   A <- (fx/ A (expt 2 combo))
                  (1 . bxl)   ; literal B <- (logxor B literal)
                  (2 . bst)   ; combo   B <- (mod combo 8)
                  (3 . jnz)   ; literal jump to literal if A !+ 0
                  (4 . bxc)   ; ignored B <- (logxor B C)
                  (5 . out)   ; combo   output combo operand mod 8
                  (6 . bdv)   ; combo   B <- (fx/ A (expt 2 combo))
                  (7 . cdv))) ; cdv     C <- (fx/ A (expt 2 combo))

(define (read-file f)
  (with-input-from-file f read))

(define (add2 x) (+ 2 x))
(define (dv A rand)
  (fxsrl A rand))
(define (run ip a b c program output)
  (define (combo rand)
    (case rand
      [(0 1 2 3) rand]
      [4 a]
      [5 b]
      [6 c]
      [else (assertion-violation 'run "invalid combo operand" rand)]))
  (call/cc
    (lambda (halt)
      (define (mem addr)
        (cond
          [(>= addr (vector-length program)) (halt (reverse! output))]
          [else (vector-ref program addr)]))
      (let ([op (cdr (assq (mem ip) opcodes))]
            [rand (mem (add1 ip))])
        ;(printf "~a ~a\n" op rand)
        (case op
          [adv (run (add2 ip) (dv a (combo rand)) b c
                    program output)]
          [bxl (run (add2 ip) a (fxlogxor b rand) c
                    program output)]
          [bst (run (add2 ip) a (fxlogand #b111 (combo rand)) c
                    program output)]
          [jnz (if (zero? a)
                 (run (add2 ip) a b c program output)
                 (run rand a b c program output))]
          [bxc (run (add2 ip) a (fxlogxor b c) c
                    program output)]
          [out (run (add2 ip) a b c
                    program (cons (fxlogand #b111 (combo rand)) output))]
          [bdv (run (add2 ip) a (dv a (combo rand)) c
                    program output)]
          [cdv (run (add2 ip) a b (dv a (combo rand))
                    program output)]
          [else (assertion-violation 'run "unexpected opcode" op)])))))

(define (main f)
  (time
    (let ([props (read-file f)])
      (display props)
      (newline)
      (let* ([a (cdr (assq 'Register-A props))]
             [b (cdr (assq 'Register-B props))]
             [c (cdr (assq 'Register-C props))]
             [program (list->vector (cdr (assq 'Program props)))]
             [output (run 0 a b c program '())])
        (when output
          (printf "output = ~a" (car output))
          (for ([n (cdr output)])
            (printf ",~a" n))
          (newline))))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
