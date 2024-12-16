#!/usr/bin/env -S scheme --program
(import (chezscheme) (util) (for) (gridvector))
(define-enumeration item
  (wall box-left box-right robot empty)
  items)

(define (gv-from-rows rows)
  (let* ([h (length rows)]
         [w (length (car rows))]
         [gv (make-gv w h)])
    (for ([y (iota h)]
          [row rows])
      (for ([x (iota w)]
            [char row])
        (gv-set! gv x y
                 (case char
                   [(#\.) (item empty)]
                   [(#\#) (item wall)]
                   [(#\[) (item box-left)]
                   [(#\]) (item box-right)]
                   [(#\@) (item robot)]
                   [else (error 'gv-from-rows "wtf" char)]))))
    gv))

(define (read-input f)
  (with-input-from-file f
    (lambda ()
      (let* ([rows (read)]
             [moves (read)])
        (values (gv-from-rows rows) moves)))))

; oops
(define (find-robot gv)
  (let outer ([y 0])
    (cond
      [(= y (gv-height gv)) (error 'find-robot "could not find it!")]
      [else
        (let inner ([x 0])
          (cond
            [(= x (gv-width gv)) (outer (add1 y))]
            [(eq? (item robot) (gv-ref gv x y))
             (values x y)]
            [else (inner (add1 x))]))])))

(define (gps x y)
  (+ (* 100 y) x))

(define (score gv)
  (let outer ([y 0] [sum 0])
    (cond
      [(= y (gv-height gv)) sum]
      [else
        (let inner ([x 0] [sum sum])
          (cond
            [(= x (gv-width gv)) (outer (add1 y) sum)]
            [(eq? (item box-left) (gv-ref gv x y))
             (inner (add1 x) (+ (gps x y) sum))]
            [else
              (inner (add1 x) sum)]))])))

(define (print-gv gv)
  (for ([y (iota (gv-height gv))])
    (for ([x (iota (gv-width gv))])
      (display
        (case (gv-ref gv x y)
          [(wall) #\#]
          [(robot) #\@]
          [(empty) #\.]
          [(box-left) #\[]
          [(box-right) #\]]
          [else (error 'print-gv "wtf" (gv-ref gv x y))])))
    (newline)))

(define (mover-x direction)
  (case direction
    [(up down) id]
    [(left) sub1]
    [(right) add1]))
(define (mover-y direction)
  (case direction
    [(up) sub1]
    [(down) add1]
    [(left right) id]))
(define (direction-axis d)
  (if (member d '(up down))
    'vertical
    'horizontal))

#|
In the wide world, we can end up with weird shapes we're pushing across the
floor, but it doesn't matter because we do it a step at a time.
+---------+---------+
|         |         |
|         |         |
|         | []    []|
| [] [] []|  [][][] |
|  []  [] |   [][]  |
|   [][]  |    []   |
|    []   |    @    |
|    @    |         |
+---------+---------+

So we loop with a list of x values.  If every x in xs is empty, we move
everything from Xsrcs into it.  Otherwise, we either fail or we recur with a
new list of xs, which could be longer or shorter.  Here's an example where the
list shortens.

+---------+---------+-------------+-------------+
|         |         | Xsrcs       | xs          |
+---------+---------+-------------+-------------+
|  []     |<-- len 2| (2 3)       | (2 3)       |
|   [][]  |<-- len 4| (3 4 5 6)   | (3 4 5 6)   |
|    []   |<-- len 2| (4 5)       | (4 5)       |
|    @    |<-- len 1| (4)         | (4)         |
+---------+---------+-------------+-------------+

In fact, when handling only the vertical case, only the y changes due to the
tick function.  The x only changes due to boxes.  This simplifies the code a
bit.
|#
; Note these two have x/y flipped to facilitate use with any? and all?
(define (empty-or-box? gv y x)
  (enum-set-member? (gv-ref gv x y)
                    (items box-left box-right empty)))
(define (item-box? gv y x)
  (enum-set-member? (gv-ref gv x y)
                    (items box-left box-right)))

(define (new-xs gv y xs1)
  (for/fold ([xs2 '()])
            ([x xs1])
    (let ([item (gv-ref gv x y)])
      (case item
        [(empty) xs2]
        [(box-left) (maybe-enqueue! (maybe-enqueue! xs2 x) (add1 x))]
        [(box-right) (maybe-enqueue! (maybe-enqueue! xs2 x) (sub1 x))]
        [else (assertion-violation 'new-xs "Unexpected item" item)]))))

(define (move-robot-vertical! gv move)
  (let ([ticky (mover-y move)])
    (let-values ([(x0 y0) (find-robot gv)])
      (let loop ([Ysrc y0] [xs (list x0)] [y (ticky y0)])
        (and (gv-legal-y? gv y)
             (all? gv-legal-x? gv xs)
             (all? empty-or-box? gv y xs)
             (cond
               [(any? item-box? gv y xs)
                (and (loop y (new-xs gv y xs) (ticky y))
                     (begin
                       (for ([x xs])
                         (gv-set! gv x y (gv-ref gv x Ysrc))
                         (gv-set! gv x Ysrc (item empty)))
                       #t))]
               [else ; base case
                 (for ([x xs])
                   (gv-set! gv x y (gv-ref gv x Ysrc))
                   (gv-set! gv x Ysrc (item empty)))
                 #t]))))))

(define (move-robot-horizontal! gv move)
  (let ([tickx (mover-x move)])
    (let-values ([(x0 y) (find-robot gv)])
      (let loop ([Xsrc x0] [x (tickx x0)])
        (and (gv-legal-x? gv x)
             (case (gv-ref gv x y)
               [(empty)
                (gv-set! gv x y (gv-ref gv Xsrc y))
                (gv-set! gv Xsrc y (item empty))
                #t]
               [(box-left box-right)
                (and (loop x (tickx x))
                     (begin
                       (gv-set! gv x y (gv-ref gv Xsrc y))
                       (gv-set! gv Xsrc y (item empty))
                       #t))]
               [else #f]))))))

(define (move-robot! gv move)
  (case move
    [(up down) (move-robot-vertical! gv move)]
    [else (move-robot-horizontal! gv move)]))


(define (main f)
  (time
    (let-values ([(gv moves) (read-input f)])
      (print-gv gv)
      (for ([move moves])
        #;(printf "~a~n" move)
        (move-robot! gv move)
        #;(print-gv gv))
        (newline)
        (newline)
        (print-gv gv)
        (printf "total=~a\n" (score gv)))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
