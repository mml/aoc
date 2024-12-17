#!/usr/bin/env -S scheme --program
(import (chezscheme) (graph) (util) (for) (gridvector))
(require-nongenerative-clause #t)
(define start #f)
(define ends '())

; BEGIN graph helpers
(define graph-find-node
  (case-lambda
    [(g name)
     (find (lambda (n) (string=? name (node-name n))) (graph-adjacency-list-nodes g))]
    [(g dir x y)
     (graph-find-node g (dir-node-name dir x y))]))
(define (dir-node-name dir x y)
  (format "--[~a (~a,~a)]--" dir x y))
(define (add-cardinal-nodes g x y)
  (for/fold ([ns '()])
            ([dir '(n e s w)])
    (let ([n (make-node-adjacency-list (dir-node-name dir x y))])
      (graph-add-node! g n)
      (cons n ns))))
(define (add-cardinal-nodes-and-edges g x y)
  ; depends on order of nodes above
  (let ([ns (add-cardinal-nodes g x y)])
    (let-list ([(w s e n) ns])
      (for ([node     (list n n e e s s w w)]
            [neighbor (list e w n s e w n s)])
        (node-add-neighbor! node neighbor 1000)))
    ns))
(define (add-start-node g x y)
  (let ([ns (add-cardinal-nodes-and-edges g x y)])
    (let-list ([(w s e n) ns])
      (set! start e))))
(define (add-end-nodes g x y)
  (set! ends (add-cardinal-nodes-and-edges g x y)))
(define (add-adjacent-edges! gv g x y)
  (for ([dir (enum-set->list compass-directions-cardinal)])
    (let ([src (graph-find-node g dir x y)])
      (when src
        (let-values ([(Xdst Ydst) (gv-neighbor-coords gv x y dir)])
          (when (and Xdst
                     (not (char=? #\# (gv-ref gv Xdst Ydst))))
            (let ([dst (graph-find-node g dir Xdst Ydst)])
              (unless dst
                (assertion-violationf
                  'add-adjacent-edges!
                  "In dir [~a] from (~a,~a) could not find (~a,~a)"
                  dir x y Xdst Ydst))
              (node-add-neighbor! src dst 1))))))))
; END graph helpers

(define (read-file f)
  (with-input-from-file f
    (lambda ()
      (let ([gv (gridvector-from-file f)]
            [g (make-graph-adjacency-list)])
        ;(print-gv gv)
        (for ([(char x y) (in-gv/indices gv)])
          (case char
            [#\# (void)]
            [#\. (add-cardinal-nodes-and-edges g x y)]
            [#\S (add-start-node g x y)]
            [#\E (add-end-nodes g x y)]
            [else
              (error 'read-file "unexpected character" char)]))
        (for ([(value x y) (in-gv/indices gv)])
          (add-adjacent-edges! gv g x y))
        ;(assert start)
        ;(assert (not (null? ends)))
        ;(display (node->struct start))
        ;(newline)
        ;(for ([end ends])
        ;  (display (node->struct end))
        ;  (newline))
        ;(display g)
        ;(newline)

        ;
        (values gv g)
        ))))

#|
(define (make-foo f)
  (let ([--- (read-file f)])
    ---))
|#
(define (main f)
  (time
    (let-values ([(gv g) (read-file f)])
      (print-gv gv)
      (display (node->struct start))
      (newline)
      (for ([end ends])
        (display (node->struct end))
        (newline))
      (let ([distances (dijkstra g start)])
        (for ([pr distances])
          (printf "~a=~a\n" (node-name (cdr pr)) (car pr)))
        (printf "minimum is ~a\n"
                (apply min (map car
                                (filter (lambda (pr) (memq (cdr pr) ends))
                                        distances)))))
      (newline))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
