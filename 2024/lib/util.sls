(library (util)
  (export id any? all?
          get-lines-from-file gridvector-from-file print-gv
          maybe-enqueue!
          range
          argmax ∘ compose flip map-with-values map-values
          gv-convolve kernel sobel
          split-string
          list-set! flip-assoc! uniq uniq!
          let-list
          numeral->number
          )
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
    ; This is a very clueless argmax.
    (let loop ([l l] [xmax #f] [fmax #f])
      (cond
        [(null? l) xmax]
        [(not xmax) (loop (cdr l) (car l) (f (car l)))]
        [else
          (let ([fx (f (car l))])
            (if (> fx fmax)
              (loop (cdr l) (car l) fx)
              (loop (cdr l) xmax fmax)))])))
  ; ∘ (Ob - U+2218 RING OPERATOR) function composition
  (define (∘ f g)
    ; TODO: with some clever define-syntax hackery, we can produce a procedure
    ; with a name like car∘cdr
    (if (procedure-known-single-valued? g)
      (lambda args
        (f (apply g args)))
      (lambda args
        (call-with-values
          (lambda () (apply g args))
          f))))
  (define compose ∘)
  (define (flip f)
    (lambda (x2 x1 . args)
      (apply f x1 x2 args)))
  (define (map-with-values pro con)
    (call-with-values pro
                      (lambda args
                        (apply map con args))))

(define-syntax map-values
  (syntax-rules ()
    [(_ con body ...)
     (map-with-values (lambda () body ...) con)]))
  (define range
    (case-lambda
      [(stop) (iota stop)]
      [(start stop)
       (let loop ([n (sub1 stop)] [l '()])
         (if (< n start)
           l
           (loop (sub1 n) (cons n l))))]
      [(start stop step)
       (do ([n start (+ n step)]
            [l '() (cons n l)])
           ((>= n stop) (reverse! l)))]))
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
  (define print-gv
    (case-lambda
      [(gv)
       (print-gv gv id)]
      [(gv entry->print-datum)
       (for ([y (iota (gv-height gv))])
         (for ([x (iota (gv-width gv))])
           (display (entry->print-datum (gv-ref gv x y))))
         (newline))]))
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

  (define (maybe-enqueue! q x)  ; Work queues
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

  (define-syntax (let-list stx)   ; structured binding
    (syntax-case stx ()
      ;[(_ ([]) body ...)
      ; #'(begin body ...)]
      [(_ ([id1] l) body ...)
       #'(let ([id1 (car l)])
           body ...)]
      [(_ ([(id1 id2) l]) body ...)
       #'(let ([id1 (car l)]
               [id2 (cadr l)])
           body ...)]
      [(_ ([(id1 id2 id3 ...) l]) body ...)
       #'(let ([id1 (car l)]
               [id2 (cadr l)])
           (let-list ([(id3 ...) (cddr l)]) body ...))]))

  (define split-string
    (case-lambda
      [(s pred-or-char)
       (let ([pred (if (char? pred-or-char)
                     (lambda (ch) (char=? ch pred-or-char))
                     pred-or-char)])
         (let loop ([s (string->list s)] [cs '()] [ss '()])
           (cond
             [(null? s)
              (reverse! (if (null? cs) ss (cons (list->string (reverse! cs)) ss)))]
             [(pred (car s))
              (if (null? cs)
                (loop (cdr s) cs ss)
                (loop (cdr s) '() (cons (list->string (reverse! cs)) ss)))]
             [else
               (loop (cdr s) (cons (car s) cs) ss)])))]
      [(s)
       (split-string s char-whitespace?)]))

  (define (list-set! l n0 obj)
    (let loop ([l l] [n n0])
      (cond
        [(null? l) (error 'list-set! "out of bounds" n0)]
        [(zero? n) (set-car! l obj)]
        [else (loop (cdr l) (sub1 n))]))
    l)

  (define (flip-assoc! l)
    (for-each (lambda (pr)
                (let ([CAR (car pr)] [CDR (cdr pr)])
                  (set-car! pr CDR)
                  (set-cdr! pr CAR)))
              l)
    l)
  (define (uniq eql? l)
    ; This works like the unix uniq utility.  That is, it only removes
    ; *adjacent* similar items.  SRFI 1 has delete-duplicates, which
    ; is quadratic.  uniq is linear and will produce a similar effect providing
    ; you don't care about order.  Which, if that's the case, maybe you should
    ; have been using some of the lset functions from that library earlier.
    (if (null? l) '()
      (let loop ([hd (car l)] [tl (cdr l)] [acc '()])
        (cond
          [(null? tl) (reverse! (cons hd acc))]
          [(eql? hd (car tl))
           (loop hd (cdr tl) acc)]
          [else (loop (car tl) (cdr tl) (cons hd acc))]))))
  (define (uniq! eql? l)
    ; This implementation inspired by filter! in SRFI 1.
    (if (null? l) '()
      (letrec ([scan-out (lambda (hd prev tl)
                           (let lp ([tl tl])
                             (if (pair? tl)
                               (if (eql? hd (car tl))
                                 (lp (cdr tl))
                                 (begin (set-cdr! prev tl)
                                        (scan-in (car tl) tl (cdr tl))))
                               (set-cdr! prev tl))))]
               [scan-in (lambda (hd prev tl)
                          (if (pair? tl)
                            (if (eql? hd (car tl))
                              (scan-out hd prev (cdr tl))
                              (scan-in (car tl) tl (cdr tl)))))])
        (scan-in (car l) l (cdr l))
        l)))
(define (numeral->number c)
  (assert (char-numeric? c))
  (- (char->integer c) (char->integer #\0)))


  (pretty-format 'let-list '(_ ([bracket (x ...) e]) #f body ...))
  );
