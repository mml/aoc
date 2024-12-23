#!/usr/bin/env -S scheme --program
(import (chezscheme)
        (only (srfi :1) lset-intersection)
        (srfi :26)                            ; cut/cute1
        (srfi :156)                           ; is
        (for)
        (graph)
        (match)
        (util)); (graph) (gridvector) (product))
(define h (make-hashtable equal-hash equal?))
(define g (make-graph-adjacency-list))
(define (find-or-create name)
  (or (hashtable-ref h name #f)
      (begin
        (let ([v (make-node-adjacency-list name)])
          (graph-add-node! g v)
          (hashtable-set! h name v)
          v))))
(define (Γ v)
  (map neighbor-dst (node-adjacency-list-neighbors v)))
(define (find-biggest-clique)
  (define Q '())
  (define Qmax '())
  (define sizemax 0)
  (let expand ([R (graph-adjacency-list-nodes g)])
    (if (null? R)
        Qmax
        (let* ([p (car R)] [Rp (lset-intersection eq? R (Γ p))])
          (set! Q (cons p Q))
          (if (null? Rp)
              (let ([size (length Q)])
                (when (is size > sizemax)
                  (set! Qmax Q)
                  (printf "new Qmax ~a\n" Q)
                  (set! sizemax size)))
              (expand Rp))
          (set! Q (remq p Q))
          (expand (cdr R))))))

(define (string-join conn . args)
  (letrec ([f (case-lambda
                [(x) x]
                [(x1 x2) (string-append x1 conn x2)]
                [(x1 x2 . rest) (string-append x1 conn x2 conn (apply f rest))])])
    (apply f args)))
(define (pretty-clique Q)
  (let ([names (map node-name Q)])
    (format "~a~n" (apply string-join "," (sort string<? names)))))

(define (main f)
  (time
    (let ([lines (get-lines-from-file f)])
      (for ([line lines])
        (let ([nodes (split-string line #\-)])
          (match nodes
            [(,a-name ,b-name)
             (let ([a (find-or-create a-name)] [b (find-or-create b-name)])
               (node-add-neighbor! a b 1)
               (node-add-neighbor! b a 1))])))
      (display (pretty-clique (find-biggest-clique)))
      (newline))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
