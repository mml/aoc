; 2d array routines
(require-nongenerative-clause #t)
(library (gridvector)
  (export
    make-gv gv-width gv-height gv-vec
    gv-ref gv-set!
    gv-copy
    gv-neighbors gv-neighbors-8 gv-legal-coords?)
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
