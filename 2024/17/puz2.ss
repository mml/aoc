#!/usr/bin/env -S scheme --program
(import (chezscheme) (util) (for))

; The main addition here is the disassembler/debugger.
; The actual solution is calculated using the code from calc.ss,
; then I double checked it using the debugger.
(define opcodes '((0 . adv)   ; combo   A <- (fx/ A (expt 2 combo))
                  (1 . bxl)   ; literal B <- (logxor B literal)
                  (2 . bst)   ; combo   B <- (mod combo 8)
                  (3 . jnz)   ; literal jump to literal if A !+ 0
                  (4 . bxc)   ; ignored B <- (logxor B C)
                  (5 . out)   ; combo   output combo operand mod 8
                  (6 . bdv)   ; combo   B <- (fx/ A (expt 2 combo))
                  (7 . cdv))) ; cdv     C <- (fx/ A (expt 2 combo))
(define randfmt '((0 . "~D")
                  (1 . "~20,,'|,:B")
                  (2 . "0~O")
                  (5 . "0~O")
                  (6 . "~D")
                  (7 . "~D")))
(define randtyp '((0 . combo)
                  (1 . literal)
                  (2 . combo)
                  (3 . addr)
                  (4 . ignored)
                  (5 . combo)
                  (6 . combo)
                  (7 . combo)))

(define (mnemonic op)
  (cdr (assq op opcodes)))

(define (read-file f)
  (with-input-from-file f read))
(define (props->state props)
  (list 0
        (cdr (assq 'Register-A props))
        (cdr (assq 'Register-B props))
        (cdr (assq 'Register-C props))
        (list->vector (cdr (assq 'Program props)))
        '()))
(define (setup f)
  (define-top-level-value 'props (read-file f))
  (define-top-level-value 'st0 (props->state (top-level-value 'props)))
  (define-top-level-value 'prog (list-ref (top-level-value 'st0) 4)))
(define (start)
  (define-top-level-value 'st (list-copy (top-level-value 'st0)))
  (apply print-state (top-level-value 'st)))
(define (next)
  (call-with-values (lambda () (apply step (top-level-value 'st)))
                    (lambda args
                      (define-top-level-value 'st (apply list args))))
  (apply print-state (top-level-value 'st)))
(define (pretty-rand op rand)
  (let ([type (cdr (assq op randtyp))]
        [fmt (assq op randfmt)])
    (case type
      [(ignored) ""]
      [(combo)
       (case rand
         [4 "A"]
         [5 "B"]
         [6 "C"]
         [else (format (cdr fmt) rand)])]
      [(literal) (format (cdr fmt) rand)]
      [(addr) (format "$~2,'0X" rand)]
      [else (assertion-violation 'pretty-rand "unexpected type" type)])))
(define (unless-null x y)
  (if (null? x) y x))
(define (go)
  (call/1cc
    (lambda (stop)
      (start)
      (display "dbg> ")
      (do ([line (get-line (current-input-port)) (get-line (current-input-port))])
          ((eof-object? line) (void))
        (let ([words (unless-null (split-string line) '(""))])
          (case (car words)
            [("" "n") (next)]
            ["q" (stop)]
            ["re" (start)]
            [("r" "run")
             ((rec run (lambda()
                         (cond
                           [(car (top-level-value 'st))
                            (call-with-values (lambda () (apply step (top-level-value 'st)))
                                              (lambda args (define-top-level-value
                                                             'st (apply list args))))
                            (run)]
                           [else
                             (apply print-state (top-level-value 'st))]))))]
            ["l" (apply print-state (top-level-value 'st))]
            [("s" "set")
             (case (cadr words)
               ["a" (list-set! (top-level-value 'st) 1 (string->number (caddr words)))]
               ["b" (list-set! (top-level-value 'st) 2 (string->number (caddr words)))]
               ["c" (list-set! (top-level-value 'st) 3 (string->number (caddr words)))]
               [else (display "set (a|b|c) <value>\n")])]
            [else (display "(n)ext (r)un (l)ist (s)et (re)start (q)uit\n")]))
        (display "dbg> ")))))
(define disassemble
  (case-lambda
  [(program)
   (disassemble program #f)]
  [(program ip)
   (for ([i (range 0 (vector-length program) 2)])
     (let ([op (vector-ref program i)]
           [rand (vector-ref program (add1 i))])
       (display (if (eq? ip i) " »" "  "))
       (printf "$~2,'0X (~d,~d) ~:@(~a~) ~a\n"
               i op rand
               (mnemonic op)
               (pretty-rand op rand))))]))
(define (print-state ip a b c program out)
  (let* ([stopped (not ip)]
         [out (if stopped out (reverse out))])
    (printf "A: ~40,,'|,:B  out: ~a\n" a out)
    (printf "B: ~40,,'|,:B\n" b)
    (printf "C: ~40,,'|,:B\n" c)
    (disassemble program ip)
    (when stopped
      (display "[not running]\n"))))
(define (add2 x) (+ 2 x))
(define (dv A rand)
  (fxsrl A rand))
(define (step ip a b c program output)
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
          [(>= addr (vector-length program))
           (halt #f a b c program
                 (reverse! output))]
          [else (vector-ref program addr)]))
      (let ([op (cdr (assq (mem ip) opcodes))]
            [rand (mem (add1 ip))])
        ;(printf "~a ~a\n" op rand)
        (case op
          [adv (values (add2 ip) (dv a (combo rand)) b c
                       program output)]
          [bdv (values (add2 ip) a (dv a (combo rand)) c
                       program output)]
          [cdv (values (add2 ip) a b (dv a (combo rand))
                       program output)]
          [bxl (values (add2 ip) a (fxlogxor b rand) c
                       program output)]
          [bst (values (add2 ip) a (fxlogand #b111 (combo rand)) c
                       program output)]
          [jnz (if (zero? a)
                 (values (add2 ip) a b c program output)
                 (values rand a b c program output))]
          [bxc (values (add2 ip) a (fxlogxor b c) c
                       program output)]
          [out (values (add2 ip) a b c
                       program (cons (fxlogand #b111 (combo rand)) output))]
          [else (assertion-violation 'run "unexpected opcode" op)])))))
(define (run ip a b c mem out)
  (if ip
    (call-with-values (lambda () (step ip a b c
                                       mem out))
                      run)
    out))


(define (main f)
  (if (string=? "go" f)
    (begin (setup "input.sexpr")
           (go))
    (time
      (let ([props (read-file f)])
        (display props)
        (newline)
        (let* ([a (cdr (assq 'Register-A props))]
               [b (cdr (assq 'Register-B props))]
               [c (cdr (assq 'Register-C props))]
               [program (cdr (assq 'Program props))]
               [mem (list->vector program)]
               [output (run 0 a b c mem '())])
          (display (run 0 117440 b c mem '()))
          (newline)
          (display program)
          (newline)
          (display (run 0 4398046511104 b c mem '()))
          (newline)
          (let loop ([a 4398046511104])
            (printf "a=~:d\r" a)
            (if (equal? program (run 0 a b c mem '()))
              (printf "a=~a\n" a)
              (loop (add1 a)))))))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
