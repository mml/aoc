; 2d array routines
(library (gridvector)
  (export
    make-gv gv-width gv-height gv-vec
    gv-ref gv-set!
    gv-copy
    gv-neighbors gv-neighbors-8 gv-legal-coords?
    with-gv-neighbors gv-neighbor-fetcher)
  (import (chezscheme))
  (define-record-type vec2
    (fields x y)
    (nongenerative))
  (define-record-type gv ; gridvector
    (fields width height vec)
    (protocol
      (lambda (new)
        (lambda (width height)
          (new width height (make-vector (fx* width height) #f)))))
    (nongenerative))
  (define (vec-offset gv x y)
    (fx+ x (fx* y (gv-width gv))))
  (define (gv-ref gv x y)
    (vector-ref (gv-vec gv) (vec-offset gv x y)))
  (define (gv-set! gv x y v)
    (vector-set! (gv-vec gv) (vec-offset gv x y) v))
  (define (gv-legal-coords? gv x y)
    (and (<= 0 x)
         (<= 0 y)
         (<= x (sub1 (gv-width gv)))
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
  (define all-directions (make-enumeration '(up down right left)))
  (define-syntax direction
    (syntax-rules (up down right left)
      [(_ up) 'up]
      [(_ down) 'down]
      [(_ right) 'right]
      [(_ left) 'left]))
  (define directions (enum-set-constructor all-directions)))
