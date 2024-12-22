#!/usr/bin/env -S scheme --program

(import (chezscheme)
        (match)
        (srfi :26)      ; (cut)
        (srfi :156)     ; (is ...)
        (for)
        )

(define slots (ash 1 20))
(define-syntax (dotimes stx)
  (syntax-case stx ()
    [(dotimes n body ...)
     #'(unless (< n 1)
         (let dot ([x (sub1 n)])
           (let ([it (begin body ...)])
             (if (zero? x)
                 it
                 (dot (sub1 x))))))]))
(define (bits x*)
  (if (null? x*)
      0
      (fxlogor (+ 9 (car x*)) (fxsll (bits (cdr x*)) 5))))
(define (unbits n x)
  (let lp ([n n] [x x] [acc '()] )
    (if (zero? n)
        (reverse! acc)
        (lp (sub1 n)
            (fxsrl x 5)
            (cons (- (fxlogand #x1f x) 9) acc)))))
(define (test-bits)
  (for* ([a (iota 19)]
         [b (iota 19)]
         [c (iota 19)]
         [d (iota 19)])
    (let ([l (list a b c d)])
      (assert (equal? l (unbits 4 (bits l)))))))
;(time (test-bits))
(define (make-table init)
  (make-vector slots init))
(define (table-update! v seq4 f)
  (let ([i (bits seq4)])
    (vector-set! v i (f (vector-ref v i)))))
(define (table-ref v seq4)
  (vector-ref v (bits seq4)))
(define table-set! vector-set!)

(define hash-seq4 bits)
(define (equal-seq4? a b)
  (andmap fx= a b))

(define prune_by 16777216)
(define (prune n)
  (mod n prune_by))
(define (secret sec)
  (let* ([a (ash sec 6)]
         [sec (prune (logxor sec a))]
         [a (ash sec -5)]
         [sec (prune (logxor sec a))]
         [a (ash sec 11)]
         [sec (prune (logxor sec a))])
    sec))
(define (secret-gen sec)
  (lambda ()
    (let ([old-sec sec])
      (set! sec (secret sec))
      old-sec)))
(define (price-gen sec)
  (let ([sg (secret-gen sec)])
    (lambda ()
      (mod (sg) 10))))
(define (delta-gen gen)
  (let ([prev (gen)])
    (lambda ()
      (let* ([cur (gen)]
             [delta (- cur prev)])
        (set! prev cur)
        `(,cur Δ ,delta)))))
(define (seq4-gen gen)
;;; Caller must subtract 4 from total number of secret numbers.  Or subtract
;;; only 3 from the number of NEW secret numbers to be generated.
  (let* ([a3 (gen)] [a2 (gen)] [a1 (gen)])
    (lambda ()
      (let* ([cur (gen)]
             [seq4 `(,(map caddr `(,a3 ,a2 ,a1 ,cur))
                      ,(car cur))])
        (set! a3 a2)
        (set! a2 a1)
        (set! a1 cur)
        seq4))))
(define (make-seq4-gen sec)
  (seq4-gen (delta-gen (price-gen sec))))


(define (iter n sec)
  (let ([g (secret-gen sec)])
    (dotimes n
      (g))))

(define (show a b c d)
  (let ([x (bits (list a b c d))])
    (printf "~32,,'|,8:B\n" x)
    (pretty-print (unbits 4 x))
    (newline)))

; For each buyer, keep track of
; 1) have we seen this sequence?
; 2) if not, add to the running sum *for* this sequence
; 3) if this is the new max, keep it
; repeat for all buyers
(define sum (make-table 0))
(define max-sum -inf.0)
(define max-seq4)
(define (sum-add! seq4 x)
  (table-update! sum seq4 (cut + <> x))
  (table-ref sum seq4))
(define (update-max! sum seq4)
  (set! max-sum sum)
  (set! max-seq4 seq4))
(define b (make-bytevector slots))
(define (do-buyer sec)
  (define seen?!
    (let ()
      (lambda (seq4)
        (let ([i (bits seq4)])
          (or (= 1 (bytevector-u8-ref b i))
              (begin
                (bytevector-u8-set! b i 1)
                #f))))))
  (bytevector-fill! b 0)
  (let ([g (make-seq4-gen sec)])
    (dotimes (- 2000 3)
      (let* ([it (g)] [seq4 (car it)] [price (cadr it)])
        (unless (seen?! seq4)
          (let ([tot (sum-add! seq4 price)])
            (when (is tot > max-sum)
              (update-max! tot seq4))))))))

(time
  (let loop ([datum (read)])
    (cond
      [(eof-object? datum)
       (printf "max-sum = ~a max-seq = ~a\n" max-sum max-seq4)]
      [else
        (do-buyer datum)
        (loop (read))])))
