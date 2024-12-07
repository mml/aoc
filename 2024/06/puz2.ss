(import (chezscheme))

#;(define-enumeration direction
  (up down right left)
  directions)
(let ()
  (define-enumeration content-item
    (clear obstacle start)
    contents)
  (define all-directions (make-enumeration '(up down right left)))
  (define-syntax direction
    (syntax-rules (up down right left)
      [(_ up) 'up]
      [(_ down) 'down]
      [(_ right) 'right]
      [(_ left) 'left]))
  (define directions (enum-set-constructor all-directions))
  (define-record gridvector
    ((immutable width) (immutable height))
    ([(immutable vec) (make-vector (* width height) #f)]))
  (define (gridvector-ref gv x y)
    (vector-ref (gridvector-vec gv) (+ x (* y (gridvector-width gv)))))
  (define (visited? gv x y)
    (let ([set (gridvector-ref gv x y)])
      (not (null? (enum-set->list set)))))
  (define (gridvector-set! gv x y v)
    (vector-set! (gridvector-vec gv) (+ x (* y (gridvector-width gv))) v))
  (define (gridvector-copy gv)
    (let* ([gv´ (make-gridvector (gridvector-width gv) (gridvector-height gv))]
           [v (gridvector-vec gv)]
           [v´ (gridvector-vec gv´)])
      ; slow
      (for-each
        (lambda (i)
          (vector-set! v´ i (vector-ref v i)))
        (iota (vector-length v))))
    gv)
  #;(define (gridvector-for-each gv f f-after-row)
    (for-each (lambda (y)
                (for-each (lambda (x)
                            (f (gridvector-ref gv x y)))
                          (iota width))
                (f-after-row))
              (iota height)))
  (define (gridvector-count-true gv)
    (let ([n 0])
      (vector-for-each (lambda (x) (when x (set! n (add1 n))))
                       (gridvector-vec gv))
      n))
  (define-record guard-state
    (x y dir))
  (define (guard-state-copy gs)
    (make-guard-state (guard-state-x gs)
                      (guard-state-y gs)
                      (guard-state-dir gs)))
  (define (char->content-item c)
    (case c
      [(#\.) (content-item clear)]
      [(#\#) (content-item obstacle)]
      [(#\^) (content-item start)]
      [else (error 'char->content-item "Unexpected character" c)]))
  (define (read-map-line)
    (let loop ([l '()] [c (read-char)])
      (if (or (eof-object? c)
              (eq? #\newline c))
        (reverse! l)
        (loop (cons (char->content-item c) l) (read-char)))))
  (define (read-map-file)
    (let ([line (read-map-line)])
      (if (null? line)
        '()
        (cons line (read-map-file)))))
  (define (map-dimensions lists)
    (values (length (car lists)) #| width |#
            (length lists) #| height |# ))
  (define (map->gridvector lists)
    (let ([x0 #f][y0 #f])
      (let-values ([(w h) (map-dimensions lists)])
        (let ([gv (make-gridvector w h)])
          (let row ([y 0] [rows lists])
            (unless (null? rows)
              (let col ([x 0] [l (car rows)])
                (unless (null? l)
                  (cond
                    [(eq? (content-item start) (car l))
                     (set! x0 x)
                     (set! y0 y)
                     (gridvector-set! gv x y (content-item clear))]
                    [else
                      (gridvector-set! gv x y (car l))])
                  (col (add1 x) (cdr l))))
              (row (add1 y) (cdr rows))))
          (values gv x0 y0)))))
  (define (make-visited gv x0 y0)
    (let ([visited (make-gridvector (gridvector-width gv) (gridvector-height gv))])
      (vector-fill! (gridvector-vec visited) (directions '()))
      (gridvector-set! visited x0 y0 (directions '(up)))
      visited))
  (define (move-off-map? gs gvmap)
    (let ([x (guard-state-x gs)][y (guard-state-y gs)][dir (guard-state-dir gs)]
          [w (gridvector-width gvmap)][h (gridvector-height gvmap)])
      (case dir
        [(up) (zero? y)]
        [(down) (= (sub1 h) y)]
        [(left) (zero? x)]
        [(right) (= (sub1 w) x)])))
  (define (obstacle? x) (eq? (content-item obstacle) x))
  (define (new-pos gs)
    (let ([x (guard-state-x gs)][y (guard-state-y gs)][dir (guard-state-dir gs)])
      (case dir
        [(up) (values x (sub1 y))]
        [(down) (values x (add1 y))]
        [(left) (values (sub1 x) y)]
        [(right) (values (add1 x) y)])))
  (define (rotate dir)
    (case dir
      [(up) (direction right)]
      [(right) (direction down)]
      [(down) (direction left)]
      [(left) (direction up)]))
  (define (guard-move! gs gvmap visited)
    (if (move-off-map? gs gvmap)
      #f
      (let-values ([(x´ y´) (new-pos gs)])
        (cond 
          [(obstacle? (gridvector-ref gvmap x´ y´))
           (set-guard-state-dir! gs (rotate (guard-state-dir gs)))]
          [else
            (set-guard-state-x! gs x´)
            (set-guard-state-y! gs y´)
            (gridvector-set!
              visited x´ y´ (enum-set-union
                                (gridvector-ref visited x´ y´)
                                (directions
                                  (list (guard-state-dir gs)))))]))))

  (define (print-visited gvmap visited)
    (for-each (lambda (y)
                (for-each (lambda (x)
                            (cond
                              [(visited? visited x y) (display (length (enum-set->list (gridvector-ref visited x y))))]
                              [(obstacle? (gridvector-ref gvmap x y))
                               (display #\#)]
                              [else (display #\.)]))
                          (iota (gridvector-width gvmap)))
                (newline))
              (iota (gridvector-height gvmap))))


  (let ([m (with-input-from-file (car (command-line-arguments)) read-map-file)])
    (let-values ([(gv x0 y0) (map->gridvector m)])
      (let ([gs (make-guard-state x0 y0 (direction up))]
            [visited (make-visited gv x0 y0)])
        (let loop ([ok (guard-move! gs gv visited)])
          (if ok
            (begin
              ;(print-visited gv visited)
              ;(newline)
              (loop (guard-move! gs gv visited)))
            (begin
              (print-visited gv visited)
              (printf "n=~a~n" (gridvector-count-true visited)))))))))
