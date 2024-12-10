; 2d array routines
(require-nongenerative-clause #t)
(library (gridvector)
  (export
    make-gv gv-width gv-height gv-vec
    gv-ref gv-set!)
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
  (define all-directions (make-enumeration '(up down right left)))
  (define-syntax direction
    (syntax-rules (up down right left)
      [(_ up) 'up]
      [(_ down) 'down]
      [(_ right) 'right]
      [(_ left) 'left]))
  (define directions (enum-set-constructor all-directions)))
