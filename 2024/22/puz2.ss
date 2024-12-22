#!/usr/bin/env -S scheme --program

(import (chezscheme)
        (match)
        (srfi :26)      ; (cut)
        (srfi :156)     ; (is ...)
        )

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
      (fxlogor (+ 9 (car x*)) (fxsll (bits (cdr x*)) 8))))
(define (unbits n x)
  (let lp ([n n] [x x] [acc '()] )
    (if (zero? n)
        (reverse! acc)
        (lp (sub1 n)
            (fxsrl x 8)
            (cons (- (fxlogand #xff x) 9) acc)))))
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
(define sum (make-hashtable hash-seq4 equal-seq4?))
(define max-sum -inf.0)
(define max-seq4)
(define (sum-add! seq4 x)
  (hashtable-update! sum seq4 (cut + <> x) 0)
  (hashtable-ref sum seq4 #f))
(define (update-max! sum seq4)
  (set! max-sum sum)
  (set! max-seq4 seq4))
(define (do-buyer sec)
  (define seen?!
    (let ([h (make-hashtable hash-seq4 equal-seq4?)])
      (lambda (seq4)
        (or (hashtable-contains? h seq4)
            (begin
              (hashtable-set! h seq4 #t)
              #f)))))
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
