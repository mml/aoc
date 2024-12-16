; 2d array routines
(library (gridvector)
  (export
    make-gv make-gv-same-size gv-width gv-height gv-vec
    gv-ref gv-set! gv-update!
    gv-copy
    gv-neighbors gv-neighbors-8 gv-legal-coords? gv-legal-x? gv-legal-y?
    with-gv-neighbors gv-neighbor-fetcher direction directions
    in-gv/indices)
  (import (chezscheme) (product))
  (define-record-type vec2
    (fields x y)
    (nongenerative))
  (define-record-type gv ; gridvector
    (fields width height vec)
    (protocol
      (lambda (new)
        (case-lambda
          [(width height)
           (new width height (make-vector (fx* width height) #f))]
          [(width height obj)
           (new width height (make-vector (fx* width height) obj))])))
    (nongenerative))
  (define make-gv-same-size
    (case-lambda
      [(gv1)
       (make-gv (gv-width gv1) (gv-height gv1))]
      [(gv1 obj)
       (make-gv (gv-width gv1) (gv-height gv1) obj)]))
  (define (vec-offset gv x y)
    (fx+ x (fx* y (gv-width gv))))
  (define (gv-ref gv x y)
    (vector-ref (gv-vec gv) (vec-offset gv x y)))
  (define (gv-set! gv x y v)
    (vector-set! (gv-vec gv) (vec-offset gv x y) v))
  (define (gv-update! gv x y f)
    (let ([v (gv-vec gv)]
          [n (vec-offset gv x y)])
      (vector-set! v n (f (vector-ref v n)))))
  (define (gv-legal-coords? gv x y)
    (and (<= 0 x)
         (<= 0 y)
         (<= x (sub1 (gv-width gv)))
         (<= y (sub1 (gv-height gv)))))
  (define (gv-legal-x? gv x)
    (and (<= 0 x)
         (<= x (sub1 (gv-width gv)))))
  (define (gv-legal-y? gv y)
    (and (<= 0 y)
         (<= y (sub1 (gv-height gv)))))
  (define (gv-neighbors gv x y)
    (filter (lambda (c)
              (apply gv-legal-coords? gv c))
            (list (list (add1 x) y)
                  (list (sub1 x) y)
                  (list x (add1 y))
                  (list x (sub1 y)))))
  (define (gv-neighbor-fetcher gv x y)
    (lambda (dx dy)
      (let ([x (+ x dx)] [y (+ y dy)])
        (and (gv-legal-coords? gv x y)
             (gv-ref gv x y)))))
  (define-syntax (with-gv-neighbors stx)
    (syntax-case stx ()
      [(k (gv x y) body ...)
       (with-implicit (k n ne e se s sw w nw)
         #'(let ([neighbor (gv-neighbor-fetcher gv x y)])
             (let ([n (neighbor 0 -1)] [ne (neighbor 1 -1)]
                   [e (neighbor 1 0)] [se (neighbor 1 1)]
                   [s (neighbor 0 1)] [sw (neighbor -1 1)]
                   [w (neighbor -1 0)] [nw (neighbor -1 -1)])
               body ...)))]))

  (define (gv-neighbors-8 gv x y)
    (filter (lambda (c)
              (apply gv-legal-coords? gv c))
            (list (list (add1 x) y)
                  (list (sub1 x) y)
                  (list x (add1 y))
                  (list x (sub1 y))
                  (list (add1 x) (add1 y))
                  (list (sub1 x) (add1 y))
                  (list (add1 x) (sub1 y))
                  (list (sub1 x) (sub1 y)))))
  (define (gv-copy gv1)
    (let ([gv2 (make-gv (gv-width gv1) (gv-height gv1))])
      (define i 0)
      (vector-for-each
        (lambda (x)
          (vector-set! (gv-vec gv2) i x)
          (set! i (add1 i)))
        (gv-vec gv1))
      gv2))
  ; a la racket
  (define (in-gv/indices gv)
    (let-values ([(xs ys) (product (iota (gv-width gv))
                                   (iota (gv-height gv)))])
      (values (map (lambda (x y) (gv-ref gv x y)) xs ys)
              xs
              ys)))

  (define all-directions (make-enumeration '(up down right left)))
  (define-syntax direction
    (syntax-rules (up down right left)
      [(_ up) 'up]
      [(_ down) 'down]
      [(_ right) 'right]
      [(_ left) 'left]))
  (define directions (enum-set-constructor all-directions)))
