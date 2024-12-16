(library (util)
  (export id any? all?
          get-lines-from-file gridvector-from-file maybe-enqueue!
          range
          argmax
          gv-convolve kernel sobel)
  (import (chezscheme)
          (gridvector)
          (for))
  (define (id x) x)
  ; all? and any? have syntax similar to apply, and semantics similar to
  ; andmap (for-all) and ormap (exists)
  (define-syntax all?
    (syntax-rules ()
      [(_ f ys) (andmap f ys)]
      [(_ f x1 ys) (andmap (lambda (y) (f x1 y)) ys)]
      [(_ f x1 x2 x3 ... ys) (andmap (lambda (y) (f x1 x2 x3 ... y)) ys)]))
  (define-syntax any?
    (syntax-rules ()
      [(_ f ys) (exists f ys)]
      [(_ f x1 ys) (exists (lambda (y) (f x1 y)) ys)]
      [(_ f x1 x2 x3 ... ys) (exists (lambda (y) (f x1 x2 x3 ... y)) ys)]))

  (define (argmax f l)
    (let loop ([l l] [xmax #f] [fmax #f])
      (cond
        [(null? l) xmax]
        [(not xmax) (loop (cdr l) (car l) (f (car l)))]
        [else
          (let ([fx (f (car l))])
            (if (> fx fmax)
              (loop (cdr l) (car l) fx)
              (loop (cdr l) xmax fmax)))])))  (define range
    (case-lambda
      [(stop) (iota stop)]
      [(start stop)
       (let loop ([n (sub1 stop)] [l '()])
         (if (< n start)
           l
           (loop (sub1 n) (cons n l))))]))
  (define (get-lines-from-file path)
    (with-input-from-file path
      (lambda ()
        (let loop ([line (get-line (current-input-port))][lines '()])
          (cond
            [(eof-object? line) (reverse! lines)]
            [else (loop (get-line (current-input-port)) (cons line lines))])))))
  (define gridvector-from-file
    (case-lambda
      [(path f)
       (let ([lines (get-lines-from-file path)])
         (let* ([w (string-length (car lines))]
                [h (length lines)]
                [g (make-gv w h)])
           (let loop ([y 0][lines lines])
             (cond
               [(null? lines) g]
               [else
                 (for-each
                   (lambda (x char)
                     (gv-set! g x y (f char)))
                   (iota w)
                   (string->list (car lines)))
                 (loop (add1 y) (cdr lines))]))))]
      [(path)
       (gridvector-from-file path id)]))
  (define-syntax kernel
    (syntax-rules ()
      [(_ (a b c) (d e f) (g h i))
       (vector (vector a b c)
               (vector d e f)
               (vector g h i))]))
  (module (sobel)
    (define Hx (kernel ( 1  0 -1)
                       ( 2  0 -2)
                       ( 1  0  1)))
    (define Hy (kernel ( 1  2  1)
                       ( 0  0  0)
                       (-1 -2 -1)))
    (define (sobel A)
      (let ([Gx (gv-convolve A Hx)]
            [Gy (gv-convolve A Hy)]
            [G (make-gv-same-size A)])
        (for* ([y (iota (gv-height G))]
               [x (iota (gv-width G))])
          (let ([gx (gv-ref Gx x y)]
                [gy (gv-ref Gy x y)])
            (gv-set! G x y
                     (sqrt (+ (* gx gx)
                              (* gy gy))))))
        G)))
  (define (gv-convolve A kern)
    (let* ([B (make-gv-same-size A 0)]
           [kh (vector-length kern)]
           [dy (fx/ kh 2)]
           [kw (vector-length (vector-ref kern 0))]
           [dx (fx/ kw 2)]
           [h (lambda (x y)
                (let ([x (- x)] [y (- y)]) ; left is right and up is down
                  (let ([x (+ x dx)] [y (+ y dy)]) ; and the middle is 0
                    (vector-ref (vector-ref kern y) x))))])
      (for* ([y (iota (gv-height A))]
             [x (iota (gv-width A))])
        ;(printf "B[~a,~a] = " y x)
        (for* ([Yk (range (- dy) (add1 dy))]
               [Xk (range (- dx) (add1 dx))])
          (let ([Xa (+ x Xk)] [Ya (+ y Yk)])
            (cond
              [(gv-legal-coords? A Xa Ya)
               #;(printf "+ ~a⋅(~a) "
                       (gv-ref A Xa Ya)
                       (h Xk Yk))
               (gv-update! B x y (lambda (n)
                                   (+ n (* (h Xk Yk)
                                           (gv-ref A Xa Ya)))))]
              #;[else
                (printf "+ 0⋅~a" (h Xk Yk))])))
        #;(printf "= ~a~n" (gv-ref B x y)))
        B))

  ; Work queues
  (define (maybe-enqueue! q x)
    (if (null? q)
      (list x)
      (let loop ([pair q][hd (car q)][tl (cdr q)])
        (cond
          [(equal? hd x) q]
          [(null? tl)
           (set-cdr! pair (list x))
           q]
          [else
            (loop tl (car tl) (cdr tl))]))))

  );
