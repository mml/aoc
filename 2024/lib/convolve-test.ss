(import (chezscheme) (util) (gridvector))

(define (test-on-test-end-diff runner)
  (let ([result (test-result-kind runner)])
    (when (eq? result 'fail)
      (let ([expected (test-result-ref runner 'expected-value)]
            [actual (test-result-ref runner 'actual-value)])
        (display "Expected: ")
        (write expected)
        (newline)
        (display "Actual:   ")
        (write actual)
        ;(newline)
        ;(display "Diff:     ")
        ;(display-diff expected actual)
        (newline)))))
(define (iota-3x3)
  (let ([A (make-gv 3 3 0)])
    (for-each (lambda (n) (vector-set! (gv-vec A) n (add1 n))) (iota 9))
    A))
(test-begin "convolve-test")
(test-runner-on-test-end! (test-runner-get) test-on-test-end-diff)

(let* ([k (kernel (1 1 1)
                  (1 0 1)
                  (1 1 1))]
       [A (iota-3x3)]
       [B (gv-convolve A k)])
  (test-equal (vector (+ 2 4 5) (+ 1 3 4 5 6) (+ 2 5 6)
                      (+ 1 2 5 7 8) (+ 1 2 3 4 6 7 8 9) (+ 2 3 5 8 9)
                      (+ 4 5 8) (+ 4 5 6 7 9) (+ 5 6 8))
              (gv-vec B)))
(let* ([k (kernel ( 0  0  0)
                  ( 1  0 -1)
                  ( 0  0  0))]
       [A (iota-3x3)]
       [B (gv-convolve A k)])
  (test-equal (vector 2 (- 3 1) -2
                      5 (- 6 4) -5
                      8 (- 9 7) -8)
              (gv-vec B)))
(let* ([k (kernel ( 0 -1  0)
                  ( 0  0  0)
                  ( 0  1  0))]
       [A (iota-3x3)]
       [B (gv-convolve A k)])
  (test-equal (vector -4 -5 -6
                      (- 1 7) (- 2 8) (- 3 9)
                      4 5 6)
              (gv-vec B)))
(let* ([k (kernel (-1 -2 -1)
                  ( 0  0  0)
                  ( 1  2  1))]
       [A (iota-3x3)]
       [B (gv-convolve A k)])
  (test-equal (vector -13 -20 -17
                      -18 -24 -18
                      13  20  17)
              (gv-vec B)))
(test-end "convolve-test")
