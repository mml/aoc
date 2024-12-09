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

(define (free-list-help v start)
  (cond
    [(= (vector-length v) start) '()]
    [(vector-ref v start)
     (free-list-help v (add1 start))]
    [else (let loop ([end start])
            (cond
              [(= (vector-length v) end) (list start (- end start))]
              [(vector-ref v end) (cons (cons start (- end start))
                                          (free-list-help v end))]
              [else (loop (add1 end))]))]))

(define (free-list v)
  (free-list-help v 0))

(define-syntax new-home
  (identifier-syntax
    [id (virtual-register 0)]
    [(set! id e) (set-virtual-register! 0 e)]))

(define (find-home fl start bytes)
  (if (null? fl)
    (begin
      (set! new-home #f)
      fl)
    (let ([n (caar fl)] [len (cdar fl)])
      (cond
        [(>= n start)
         (set! new-home #f)
         fl]
        [(< len bytes) (cons (car fl)
                             (find-home (cdr fl) start bytes))]
        [else
          (set! new-home n)
          (if (= len bytes)
            (cdr fl)
            (cons (cons (+ n bytes) (- len bytes)) (cdr fl)))]))))

(define (move! v fl src dst len)
  (let byte ([src src] [dst dst] [len len])
    (unless (zero? len)
      (vector-set! v dst (vector-ref v src))
      (vector-set! v src #f)
      (byte (add1 src) (add1 dst) (sub1 len)))))

(define (compact v fl)
  (compact-help v fl (sub1 (vector-length v))))
(define (compact-help v fl r)
  (let right ([r r])
    (cond
      [(negative? r) v]
      [(not (vector-ref v r)) (right (sub1 r))]
      [else
        (let left ([l (sub1 r)])
          (cond
            [(or (= l -1)
                 (not (eq? (vector-ref v l) (vector-ref v r))))
             (let ([len (- r l)])
               (let ([fl′ (find-home fl (add1 l) len)])
                 (when new-home
                   (move! v fl (add1 l) new-home len))
                 (compact-help v fl′ l) ; should keep track of r
                 ))]
            [else (left (sub1 l))]))])))

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
  (let* ([v (make-block-vector fname)]
         [f (free-list v)])
    (printf "~a~n" (checksum (compact v f)))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
