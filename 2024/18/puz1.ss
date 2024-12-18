#!/usr/bin/env -S scheme --program
(import (chezscheme) (util) (for) (gridvector) (graph))

(define (read-file f)
  (with-input-from-file f
    (lambda ()
      (let loop ([line (get-line (current-input-port))] [l '()])
        (if (eof-object? line)
          (reverse! l)
          (let-list ([(x y) (split-string line #\,)])
            (loop (get-line (current-input-port))
                  (cons (map string->number (list x y)) l))))))))

(module (graph-from-gv)
  (define (graph-from-gv gv make-node? connect-neighbor?)
    (let ([g (make-graph-adjacency-list)]
          [h (make-hashtable equal-hash equal?)])
      (for ([(ch x y) (in-gv/indices gv)])
        (let ([name (make-node? ch x y)])
          (when name
            (let ([node (make-node-adjacency-list name)])
              (node-prop-set! node 'x x)
              (node-prop-set! node 'y y)
              (node-prop-set! node 'ch ch)
              (hashtable-set! h (list x y) node)
              (graph-add-node! g node)))))
      (let-values ([(c* node*) (hashtable-entries h)])
        (do ([i 0 (add1 i)])
            ((= i (vector-length c*)))
          (let ([c1 (vector-ref c* i)]
                [node1 (vector-ref node* i)])
            (for ([c2 (apply gv-neighbors gv c1)])
              (let ([node2 (hashtable-ref h c2 #f)])
                (when node2
                  (let-values ([(x1 y1) (apply values c1)]
                               [(x2 y2) (apply values c2)])
                    (when (connect-neighbor? x1 y1 x2 y2)
                      (node-add-neighbor! node1 node2 1)))))))))
      (values g h))))

(define (main f n w h)
  (time
    (let* ([coords (read-file f)]
           [n (string->number n)]
           [w (string->number w)]
           [h (string->number h)]
           [gv (make-gv w h #\.)])
      (for ([c (list-head coords n)])
        (let-list ([(x y) c])
          (gv-set! gv x y #\#)))
      (let-values  ([(g ht)
                     (graph-from-gv
                       gv
                       (lambda (ch x y)
                         (and (char=? #\. ch)
                              (format "--[~a,~a]--" x y)))
                       (lambda (x1 y1 x2 y2) #t))])
        (printf "~a\n" gv)
        (for ([v (graph-adjacency-list-nodes g)])
          (display (node->struct v))
          (newline))
        (let ([dist (flip-assoc! (dijkstra g (hashtable-ref ht '(0 0) #f)))]
              [dst (hashtable-ref ht (list (sub1 w) (sub1 h)) #f)])
          (display (cdr (assoc dst dist)))
          (newline)
        )))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
