(import (chezscheme))

#;(define-enumeration direction
  (up down right left)
  directions)
(time
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
  #|
  ; Instead of separate x, y, and dir parameters, next time I might try these
  ; records, which would mean I could treat the guard as just her location.
  (define-record coord
    (x y))
  (define-record garde coord
    (dir))
  ; And then I could alwo write
  (gridvector-set! gv loc x)
  ; I also think a few helpers would be useful shorthand
  (define X coord-x)
  (define Y coord-y)
  (define DIR garde-dir)
  (define W gridvector-width)
  (define H gridvector-height)
  ; And also these almost-"deconstructing bindings"
  (let-coord [x y] ...)
  (let-garde [x y dir] ...)
  |#
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
        (iota (vector-length v)))
      gv´))
  (define (gridvector-for-each gv f after-row-thunk)
    (for-each (lambda (y)
                (for-each (lambda (x)
                            (f x y (gridvector-ref gv x y)))
                          (iota (gridvector-width gv)))
                (after-row-thunk))
              (iota (gridvector-height gv))))
  (define (iota2a a b)
    (let loop ([i 0] [j 0])
      (cond
        [(= b j) '()]
        [(= a i) (loop 0 (add1 j))]
        [else (cons i (loop (add1 i) j))])))
  (define (iota2b a b)
    (let loop ([i 0] [j 0])
      (cond
        [(= b i) '()]
        [(= a j) (loop (add1 i) 0)]
        [else (cons i (loop i (add1 j)))])))
  (define (gridvector-map gv f)
    (let ([w (gridvector-width gv)][h (gridvector-height gv)])
      (map (lambda (x y)
             (f x y (gridvector-ref gv x y)))
           (iota2a w h)
           (iota2b w h))))
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
  (define (for/visited v f)
    (for-each (lambda (x)
                (for-each (lambda (y)
                            (for-each (lambda (dir)
                                        (f x y dir))
                                      (enum-set->list (gridvector-ref v x y))))
                          (iota (gridvector-height v))))
              (iota (gridvector-width v))))
  (define move-off-map?
    (case-lambda
      [(x y dir gvmap)
       (let ([w (gridvector-width gvmap)][h (gridvector-height gvmap)])
         (case dir
           [(up) (zero? y)]
           [(down) (= (sub1 h) y)]
           [(left) (zero? x)]
           [(right) (= (sub1 w) x)]))]
      [(gs gvmap)
       (let ([x (guard-state-x gs)][y (guard-state-y gs)][dir (guard-state-dir gs)])
         (move-off-map? x y dir gvmap))]))
  (define (obstacle? x) (eq? (content-item obstacle) x))
  (define new-pos
    (case-lambda
      [(x y dir)
       (case dir
         [(up) (values x (sub1 y))]
         [(down) (values x (add1 y))]
         [(left) (values (sub1 x) y)]
         [(right) (values (add1 x) y)])]
      [(gs)
       (let ([x (guard-state-x gs)][y (guard-state-y gs)][dir (guard-state-dir gs)])
         (new-pos x y dir))]))
  (define (rotate dir)
    (case dir
      [(up) (direction right)]
      [(right) (direction down)]
      [(down) (direction left)]
      [(left) (direction up)]))
  (define (check-guard-move! f gs gvmap visited)
    (if (move-off-map? gs gvmap)
      #f
      (let-values ([(x´ y´) (new-pos gs)])
        (cond
          [(obstacle? (gridvector-ref gvmap x´ y´))
           (let ([dir´ (rotate (guard-state-dir gs))])
             (if (f (guard-state-x gs) (guard-state-y gs) dir´)
               'loop
               (set-guard-state-dir! gs (rotate (guard-state-dir gs)))))]
          [(f x´ y´ (guard-state-dir gs)) 'loop]
          [else
            (set-guard-state-x! gs x´)
            (set-guard-state-y! gs y´)
            (gridvector-set!
              visited x´ y´ (enum-set-union
                                (gridvector-ref visited x´ y´)
                                (directions
                                  (list (guard-state-dir gs)))))
            (void)]))))

  (define (guard-move! gs gvmap visited)
    (check-guard-move! (lambda (x y dir) #f) gs gvmap visited))
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
  (define (loops? gv gs0)
    (let ([gs (guard-state-copy gs0)]
          [visited (make-visited gv (guard-state-x gs0) (guard-state-y gs0))])
      (let ([f (lambda (x y dir)
                 (let ([v (gridvector-ref visited x y)])
                   (cond
                     [(enum-set-member? dir v)
                      #;(newline)
                      ;(print-visited gv visited)
                      #;(printf "Loop detected at (~a,~a), already been here going ~a (~a)~n"
                              x y dir (enum-set->list v))
                      #t]
                     [else #f])))])
        (let loop ([n 0][ok (check-guard-move! f gs gv visited)])
          ;(printf "loop ~a ~a     ~C" n obs-count #\return)
          (case ok
            [(loop) #t]
            [(#f) #f]
            [else (loop (add1 n) (check-guard-move! f gs gv visited))])))))
  ; This clearly has a bug in it.  It misses 30 locations on the large input.
  ; However, it turns out the code is fast enough to just try every location.
  ; On my workstation, this took about 12s without optimizations, or 8s with
  ; them.
  (define (generate-maybes gv visited gs0)
    (let ([i 0][off 0][already 0][start 0]
          [locations (make-gridvector (gridvector-width gv) (gridvector-height gv))])
      (for/visited visited (lambda (x y dir)
                             (printf
                               "generate-maybes ~a off=~a already=~a start=~a~C"
                               i off already start #\return)
                             (flush-output-port)
                             (set! i (add1 i))
                             (if (move-off-map? x y dir gv)
                               (begin
                                 (set! off (add1 off))
                                 (void))
                               (let-values ([(x´ y´) (new-pos x y dir)])
                                 (cond
                                   [(obstacle? (gridvector-ref gv x´ y´))
                                    (set! already (add1 already))
                                    (void)]
                                   [(and (= (guard-state-x gs0) x´)
                                         (= (guard-state-y gs0) y´))
                                    (set! start (add1 start))
                                    (void)]
                                   [else
                                     (gridvector-set! locations x´ y´ #t)])))))
      (printf
        "generate-maybes ~a off=~a already=~a start=~a~C"
        i off already start #\return)
      (newline)
      locations))
  (define obs-count 0)
  (define (make-try-obstruction gv0 gs0)
    (let ([gv (gridvector-copy gv0)])
      (lambda (x y)
        ;(printf "try-obstruction ~a~C" obs-count #\return)
        (set! obs-count (add1 obs-count))
        (gridvector-set! gv x y (content-item obstacle))
        (let ([result (loops? gv gs0)])
          (gridvector-set! gv x y (gridvector-ref gv0 x y)) ; restore
          result))))
  (define (count/maybe gv0 locations gs0)
    (let* ([try-obstruction (make-try-obstruction gv0 gs0)]
           [worked (gridvector-map
                     locations (lambda (x y v)
                                 (and v
                                      (try-obstruction x y))))])
      (newline)
      (fold-left (lambda (n x) (if x (add1 n) n)) 0 worked)))

  (let ([m (with-input-from-file (car (command-line-arguments)) read-map-file)])
    (let-values ([(gv x0 y0) (map->gridvector m)])
      (let* ([gs0 (make-guard-state x0 y0 (direction up))]
             [visited (make-visited gv x0 y0)]
             [gs (guard-state-copy gs0)])
        (loops? gv gs)
        (let loop ([ok (guard-move! gs gv visited)])
          (if ok
            (begin
              ;(print-visited gv visited)
              ;(newline)
              (loop (guard-move! gs gv visited)))
            (begin
              ;(print-visited gv visited)
              (let ([maybes (generate-maybes gv visited gs0)]
                    [all (make-gridvector (gridvector-width gv)
                                          (gridvector-height gv))])
                (vector-fill! (gridvector-vec all) #t)
                (printf "count=~a~n" (count/maybe gv all gs0)))
              (printf "n=~a~n" (gridvector-count-true visited))))))))))
