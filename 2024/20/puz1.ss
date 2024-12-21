#!/usr/bin/env -S scheme --program
(import (chezscheme)
        (only (srfi :1) count)
        (srfi :2)     ; and-let*
        (srfi :26)    ; cut/cute
        (srfi :156)   ; is
        (for) (util) (graph) (gridvector))

(define g)
(define start)
(define end)

(define wall? (cut char=? #\# <>))
(define (track? x)
  (or (char=? x #\.)
      (char=? x #\S)
      (char=? x #\E)))

(define (make-props gv x y ch)
  (let* ([node (make-node-adjacency-list (format "(~a,~a)" x y))])
    (cond
      [(char=? #\S ch) (set! start node)]
      [(char=? #\E ch) (set! end node)])
    (node-set-prop! node 'x x)
    (node-set-prop! node 'y y)
    (node-set-prop! node 'ch ch)
    (graph-add-node! g node)
    `((node . ,node))))

(define (add-edges! gv)
  (for ([(ch x y) (in-gv/indices gv)])
    (unless (char=? #\# ch)
      (for ([(ch2 x2 y2) (in-gv-neighbors/indices gv x y)])
        (unless (char=? #\# ch2)
          (node-add-neighbor! (gv-prop-value gv x y 'node)
                              (gv-prop-value gv x2 y2 'node)
                              1))))))

(define (gv-cost gv x y)
  (node-prop-value (gv-prop-value gv x y 'node) 'cost))

(define (analyze-cheat gv Xlef Ylef Xrig Yrig)
  ;;; ANALYZE-CHEAT returns (values SAV X2 Y2).
  ;;; SAV is the savings.  X2 and Y2 are the section of track the cheater will
  ;;; exit ONTO.
  (let ([Clef (gv-cost gv Xlef Ylef)] [Crig (gv-cost gv Xrig Yrig)])
    (if (is Clef < Crig)
      (values (- Crig Clef 2) Xrig Yrig)
      (values (- Clef Crig 2) Ylef Ylef))))
(define (cheats-for-position gv x y)
  (let ([nbr-ch (gv-neighbor-fetcher gv x y)])
    (for/fold ([cheats '()])
      ([lef '(n e)] [rig '(s w)])
      (if (and-let* ([CHlef (nbr-ch lef)]
                     [(track? CHlef)]
                     [CHrig (nbr-ch rig)]
                     [(track? CHrig)]))
        (let-values ([(Xlef Ylef) (gv-neighbor-coords gv x y lef)]
                     [(Xrig Yrig) (gv-neighbor-coords gv x y rig)])
          (let-values ([(sav x2 y2)
                        (analyze-cheat gv Xlef Ylef Xrig Yrig)])
            ;(printf "sav ~a\n" sav)
            (cons
              `(cheat ,sav (,x ,y) (,x2 ,y2))
              cheats)))
        cheats))))

(define (cheats gv)
  (let-values ([(chs xs ys) (in-gv/indices gv)]) ; My impoverished for/fold.
    (for/fold ([cheats '()])
              ([ch chs] [x xs] [y ys])
      (if (track? ch)
        cheats
        (append (cheats-for-position gv x y) cheats)))))

(define (count-cheats gv)
  (define sum 0)
  (for ([(ch x y) (in-gv/indices gv)])
    (when (wall? ch)
      (set! sum
        (+ sum
           (with-gv-neighbors (gv x y)
             (length (filter id (list (and (track? e) (track? w))
                                      (and (track? n) (track? s))))))))))
  sum)

(define (main f)
  (set! g (make-graph-adjacency-list))
  (time
    (let ([gv (gridvector-from-file f id make-props)])
      (add-edges! gv)
      (dijkstra! g 'cost start)
      (let ([cheats (cheats gv)])
        ;(print-gv gv)
        (newline)
        (printf "shortest path = ~a\n" (node-prop-value end 'cost))
        ;(printf "total possible cheats = ~a\n" (count-cheats gv))
        (printf "total possible cheats = ~a\n" (length cheats))
        (printf "cheats >= 100 ps = ~a\n"
                (count (cut >= <> 100) (map cadr cheats)))
        #;(let ([counts (counts-by cadr cheats)])
          (for ([pr (sort car-< counts)])
            (printf "~d cheat~:p => ~d picosecond~:p\n"
                    (cdr pr) (car pr)))
        )))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
