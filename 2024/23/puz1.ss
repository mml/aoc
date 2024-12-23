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

(define (find-triangles)
  (let outer ([nodes (graph-adjacency-list-nodes g)] [triangles '()])
    (if (null? nodes)
        triangles
        (let ([u (car nodes)])
          (let middle ([v* (map neighbor-dst (node-adjacency-list-neighbors u))]
                       [triangles triangles])
            (if (null? v*)
                (outer (cdr nodes) triangles)
                (let ([v (car v*)])
                  (if (is (node-name v) string<=? (node-name u))
                      (middle (cdr v*) triangles)
                      (let inner ([common-neighbors
                                    (lset-intersection eq?
                                      (map neighbor-dst (node-adjacency-list-neighbors u))
                                      (map neighbor-dst (node-adjacency-list-neighbors v)))]
                                  [triangles triangles])
                        (if (null? common-neighbors)
                            (middle (cdr v*) triangles)
                            (let ([w (car common-neighbors)])
                              (if (is (node-name w) string<=? (node-name v))
                                  (inner (cdr common-neighbors) triangles)
                                  (inner (cdr common-neighbors)
                                         (cons `(,u ,v ,w) triangles))))))))))))))

; (define (read-file f)
;   (with-input-from-file f
;     (lambda ()
;       ; loop probably based on one of
;       ; (let loop ([line (get-line (current-input-port))] [lines '()])
;       ; (let loop ([datum (read)] [acc '()])
;       ; (let loop ([ch (read-char)] [acc '()])
;       ;
;       ;   (cond
;       ;     [(eof-object? XXX) (reverse! YYY)]
;       ;     [--- ---]
;       ;     [else ---])
;       ;   --- )
;       ---)))

; (define (make-foo f)
;   (let ([--- (read-file f)])
;     ---))

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
      (for ([tri (find-triangles)])
        (apply printf "~a,~a,~a\n" (map node-name tri))))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
