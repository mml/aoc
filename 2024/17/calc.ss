#!/usr/bin/env -S scheme --script

(import (for) (srfi :64))

;(print-radix 2)
; given n, to encode, and idx an offset, return
; _code_, the 3 bit code, and _ptr_ the 3 bit value that points to it
(define (f n idx)
  (values (logxor n #b100 idx) (logxor idx #b1)))

(define (show-bits-and-mask n idx)
  (printf "(bits-and-mask ~a ~a)\n" n idx)
  (let-values ([(code ptr) (f n idx)])
    (printf "code = ~3,'0B\n" code)
    (printf " ptr = ~3,'0B\n" ptr)
    (let* ([code-bits (ash code idx)]
           [code-mask (ash #b111 idx)]
           [ptr-mask #b111]
           [mask (logor code-mask ptr-mask)])
      (printf "~15,,'|,:B code-mask\n" code-mask)
      (printf "~15,,'|,:B ptr-mask\n" ptr-mask)
      (printf "~15,,'|,:B mask\n" mask)
      (printf "~15,,'|,:B code-bits & mask\n" (logand code-bits mask))
      (let ([diff (logxor (logand code-bits ptr-mask)
                          (logand ptr code-mask))])
        (unless (zero? diff)
          (printf "~15,,'|,:B CONFLICT\n" diff)))
      )))

(define (bits-and-mask n idx)
  (let-values ([(code ptr) (f n idx)])
    (let* ([code-bits (ash code idx)]
           [code-mask (ash #b111 idx)]
           [ptr-mask #b111]
           [mask (logor code-mask ptr-mask)]
           [diff (logxor (logand code-bits ptr-mask)
                         (logand ptr code-mask))])
      (if (zero? diff) (values (logor code-bits ptr) mask)
        (values #f #f)))))

(define (bitses-and-masks out)
  (for/fold ([bitses '()]
             [masks '()])
            ([shift (iota 8)])
    (let-values ([(bits mask) (bits-and-mask out shift)])
      (if bits
        (values (cons bits bitses) (cons mask masks))
        (values bitses masks)))))


(define (encode out shift)
  (let* ([lo (logxor shift 1)]
         [hi (logxor out #b100 shift)])
    ;(printf "~a (>> ~a) = [~a...~a]\n" out shift hi lo)
    (if (< shift 3)
      #f
      (logor (ash hi shift) lo))))

(let ([test-on-test-end-diff
        (lambda (runner)
          (let ([result (test-result-kind runner)])
            (when (eq? result 'fail)
              (let ([err (test-result-ref runner 'actual-error)]
                    [expected (test-result-ref runner 'expected-value)]
                    [actual (test-result-ref runner 'actual-value)])
                (when err
                  (display "Error: ")
                  (write err)
                  (newline))
                (display "Expected: ")
                (write expected)
                (newline)
                (display "Actual:   ")
                (write actual)
                ;(newline)
                ;(display "Diff:     ")
                ;(display-diff expected actual)
                (newline)))))])

(test-begin "encode-test")
  ;(test-runner-on-test-end! (test-runner-get) test-on-test-end-diff)
(for* ([out (iota 8)]
       [shift (iota 8)])
  (let-values ([(bits mask) (bits-and-mask out shift)])
    (let ([A (encode out shift)]
          [B #f]
          [C #f])
      (cond
        [(not A) (test-assert "encode fails only if shift<3" (< shift 3))
                 #;(test-assert "y" (not bits))
                 #;(test-assert "z" (not mask))]
        [(or (not bits)
             (not mask))
         (test-assert "bits-and-mask should fail when encode does" (not A))]
        [else
          (test-equal bits A)
          (test-equal (logand bits mask) A)
          (set! B (mod A 8))
          (test-equal (logxor 1 shift) B)
          (test-equal shift (logxor B 1))
          (set! B (logxor 1 B))
          (set! C (ash A (- B)))
          (set! B (logxor B C))
          (set! B (logxor B 4))
          (test-equal out B)]))))
(test-end "encode-test"))


(define (gen-n n)
  (for/fold ([A* '()])
            ([idx (iota 8)])
    (let-values ([(bits mask) (bits-and-mask n idx)])
      (if bits
        (cons bits A*)
        A*)
      #;(cond
        [(encode n idx)
         => (lambda (x) (cons x A*))]
        [else A*]))))


(define (merge-A bits1 mask1 bits2 mask2)
  (let ([overlap-mask (logand mask1 mask2)])
    (if (or (zero? overlap-mask)
            (let* ([overlap1 (logand overlap-mask bits1)]
                   [overlap2 (logand overlap-mask bits2)])
              (zero? (logxor overlap1 overlap2))))
      (values (logor bits1 bits2) (logor mask1 mask2))
      (values #f #f))))

(define (gen-candidates A0 MASKa0 out)
  (let ([A1 (ash A0 3)]
        [MASKa1 (ash MASKa0 3)])
    (let-values ([(bitses masks) (bitses-and-masks out)])
      (for/fold ([A* '()]
                 [MASKa* '()])
                ([bits bitses]
                 [mask masks])
        (let-values ([(A2 MASKa2) (merge-A A1 MASKa1 bits mask)])
          (if A2
            (values (cons A2 A*) (cons MASKa2 MASKa*))
            (values A* MASKa*)))))))

(define (map-values f a* b*)
  (let loop ([a* a*] [b* b*] [c* '()] [d* '()])
    (if (null? a*)
      (values (reverse! c*) (reverse! d*))
      (call-with-values
        (lambda () (f (car a*) (car b*)))
        (lambda (c d) (loop (cdr a*) (cdr b*) (cons c c*) (cons d d*)))))))

(define (gen-list outs)
  (let outer ([A* '(0)] [m* '(0)] [outs outs])
    (if (null? outs)
      (values A* m*)
      (let ([out (car outs)])
        (call-with-values
          (lambda ()
            (call-with-values (lambda ()
                                (map-values (lambda (A m)
                                              (gen-candidates A m out)) A* m*))
                              (lambda (A** m**)
                                (values (apply append A**)
                                        (apply append m**)))))
          (lambda (A* m*)
            (outer A* m* (cdr outs))))))))

(define (gen-masked outs)
  (let-values ([(A* m*) (gen-list outs)])
    (sort < (map logand A* m*))))


(let ([props (with-input-from-file "input.sexpr" (lambda () (read)))])
  (display (gen-masked (reverse (cdr (assq 'Program props)))))
  (newline))

