#!/usr/bin/env -S scheme --program
(import (chezscheme) (util) (for) (gridvector))
(define-enumeration item
  (wall box robot empty)
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
                   [(#\O) (item box)]
                   [(#\@) (item robot)]
                   [else (error 'gv-from-rows "wtf")]))))
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
            [(eq? (item box) (gv-ref gv x y))
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
          [(box) #\O]
          [else (error 'print-gv "wtf" (gv-ref gv x y))])))
    (newline)))

(define (mover-x direction)
  (case direction
    [(up) id]
    [(down) id]
    [(left) sub1]
    [(right) add1]))
(define (mover-y direction)
  (case direction
    [(up) sub1]
    [(down) add1]
    [(left) id]
    [(right) id]))

(define (move-robot! gv move)
  (let ([tickx (mover-x move)] [ticky (mover-y move)])
    (let-values ([(x0 y0) (find-robot gv)])
      (let loop ([Xsrc x0] [Ysrc y0] [x (tickx x0)] [y (ticky y0)])
        (if (gv-legal-coords? gv x y)
          (case (gv-ref gv x y)
            [(empty)
             (gv-set! gv x y (gv-ref gv Xsrc Ysrc))
             (gv-set! gv Xsrc Ysrc (item empty))
             #t]
            [(box)
             (cond
               [(loop x y (tickx x) (ticky y))
                (gv-set! gv x y (gv-ref gv Xsrc Ysrc))
                (gv-set! gv Xsrc Ysrc (item empty))
                #t]
               [else #f])]
            [(wall) #f]
            [(robot) (error 'move-robot! "found myself!" x y)]
            [else (error 'move-robot! "unknown object" x y)])
          #f)))))


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
