#!/usr/bin/env -S scheme --program
(import (chezscheme) (gridvector) (product))
(define (read-file f)
  (with-input-from-file f
    (lambda ()
      (let loop ([line (get-line (current-input-port))][lines '()])
        (cond
          [(eof-object? line) (reverse! lines)]
          [else (loop (get-line (current-input-port)) (cons line lines))])))))
(define (make-map f)
  (let ([lines (read-file f)])
    (let* ([w (string-length (car lines))]
           [h (length lines)]
           [g (make-gv w h)])
      (let loop ([y 0][lines lines])
        (cond
          [(null? lines) g]
          [else
            (for-each
              (lambda (x char)
                (gv-set! g x y char))
              (iota w)
              (string->list (car lines)))
            (loop (add1 y) (cdr lines))])))))
(define (maybe-enqueue q x)
  (if (member x q)
    q
    (cons x q)))
(define (corners gv x y char)
  (let ([neighbor (lambda (dx dy)
                    (let ([x (+ x dx)] [y (+ y dy)])
                      (if (gv-legal-coords? gv x y)
                        (gv-ref gv x y)
                        #f)))]
        [outside-corner (lambda (a b)
                          (if (not (or (eq? a char)
                                       (eq? b char)))
                            1
                            0))]
        [inside-corner (lambda (a b c)
                         (if (and (eq? a char)
                                  (eq? b char)
                                  (not (eq? c char)))
                           1
                           0))])
    (let ([n (neighbor 0 -1)] [s (neighbor 0 1)]
          [e (neighbor 1 0)] [w (neighbor -1 0)]
          [ne (neighbor 1 -1)] [nw (neighbor -1 -1)]
          [se (neighbor 1 1)] [sw (neighbor -1 1)])
      (apply +
             (append (map outside-corner
                          (list n n s s)
                          (list e w e w))
                     (map inside-corner
                          (list n n s s)
                          (list e w e w)
                          (list ne nw se sw)))))))
(define (find-region! gv counted x y)
  (if (gv-ref counted x y)
    0
    (let ([char (gv-ref gv x y)])
      (let loop ([queue (list (list x y))] [area 0] [c 0])
        (if (null? queue)
          (begin
            ;(printf "~c ~a * ~a = ~a~n" char area c (* area c))
            (* area c))
          (let ([x (caar queue)] [y (cadar queue)])
            (cond
              [(eq? char (gv-ref gv x y))
               (gv-set! counted x y #t)
               (loop
                 (fold-left
                   maybe-enqueue
                   (cdr queue)
                   (filter (lambda (c)
                             (not (apply gv-ref counted c)))
                           (gv-neighbors gv x y)))
                 (add1 area)
                 (+ c (corners gv x y char)))]
              [else
                (loop (cdr queue) area c)])))))))
(define (find-all-regions! gv)
  (let ([counted (make-gv (gv-width gv) (gv-height gv))])
    (let-values ([(xs ys) (product (iota (gv-width gv))
                                   (iota (gv-height gv)))])
      (apply
        + (map (lambda (x y)
                 (find-region! gv counted x y))
               xs ys)))))

(define (main f)
  (time
    (let ([gv (make-map f)])
      (let ([total (find-all-regions! gv)])
        (printf "~a~n" total)))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
