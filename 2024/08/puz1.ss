#!/usr/bin/env -S scheme --program
(import (chezscheme) (product))

(define W)
(define H)

(define-record coord (x y))
(define (coord- a b)
  (make-coord (- (coord-x a) (coord-x b))
              (- (coord-y a) (coord-y b))))
(define (coord+ a b)
  (make-coord (+ (coord-x a) (coord-x b))
              (+ (coord-y a) (coord-y b))))
(define (coord-negate c)
  (make-coord (- (coord-x c)) (- (coord-y c))))
(define (coord-oob? c)
  (or (negative? (coord-x c))
      (> (coord-x c) (sub1 W))
      (negative? (coord-y c))
      (> (coord-y c) (sub1 H))))
(define (coord= a b)
  (and (= (coord-x a) (coord-x b))
       (= (coord-y a) (coord-y b))))

(define (make-coord-set)
  (make-hashtable
    (lambda (c) (+ (ash (coord-x c) 20) (coord-y c)))
    coord=))
(define (set-insert! s elt)
  (hashtable-set! s elt #t))
(define (set-remove! s elt)
  (hashtable-delete! s elt))
(define (set->list s)
  (vector->list (hashtable-keys s)))

(define (read-file)
  (let ([h (make-eq-hashtable)])
    (let loop ([l (get-line (current-input-port))] [y 0])
      (cond
        [(eof-object? l)
         (set! H y)
         h]
        [else
          (let ([len (string-length l)])
            (set! W len)
            (for-each
              (lambda (c x)
                (case c
                  [(#\.) (void)]
                  [else (eq-hashtable-update!
                          h
                          c
                          (lambda (l)
                            (cons (make-coord x y) l))
                          '())]))
              (string->list l)
              (iota len)))
          (loop (get-line (current-input-port)) (add1 y))]))))

(define (add-antinodes! set a b)
  (let* ([a-b (coord- a b)]
         [c (coord+ a a-b)]
         [d (coord- b a-b)])
    (unless (coord-oob? c)
      (set-insert! set c))
    (unless (coord-oob? d)
      (set-insert! set d))))

(define (countem h)
  (let ([set (make-coord-set)])
    (let outer ([keys (vector->list (hashtable-keys h))])
      (cond
        [(null? keys) (length (set->list set))]
        [else
          (let ([coords (eq-hashtable-ref h (car keys) #f)])
            (let loop ([c (car coords)] [coords (cdr coords)])
              (cond
                [(null? coords) (outer (cdr keys))]
                [else
                  (call-with-values
                    (lambda () (product (list c) coords))
                    (lambda (as bs)
                      (map (lambda (a b)
                             (add-antinodes! set a b)) as bs)))
                  (loop (car coords) (cdr coords))])))]))))

(define (input)
  (define-top-level-value 'h (with-input-from-file "input.txt" read-file)))
(define h (with-input-from-file (car (command-line-arguments)) read-file))
(printf "~a~n" (countem h))
