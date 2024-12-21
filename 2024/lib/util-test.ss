(import (chezscheme) (util))

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

  (test-runner-on-test-end! (test-runner-get) test-on-test-end-diff)
  (test-begin "map-with-values")
  (let ()
    (import (product))

    (test-equal "base assertion"
                30
                (apply +
                       (call-with-values (lambda () (product (iota 5) (iota 3)))
                                         (lambda (as bs)
                                           (map * as bs)))))
    (test-equal "map-with-values"
                30
                (apply +
                       (map-with-values (lambda () (product (iota 5) (iota 3)))
                                        *)))

    (test-equal "map-values"
                30
                (apply + (map-values * (product (iota 5) (iota 3)))))
    )

  (test-end "map-with-values")

  (test-skip "slow") ; Comment out when you really need to be sure.
  (test-begin "uniq")
  (let ()
    (for-each (lambda (f)
                (test-equal
                  '(0 1 2 3 4)
                  (f = '(0 0 0 0 1 1 1 1 1 2 3 3 3 3 3 4 4))))
              (list uniq uniq!))

    (for-each (lambda (f)
                (test-equal
                  '(1 0 1 2 3 4)
                  (f = '(1 0 1 1 1 1 1 1 2 2 2 2 2 2 2 3 3 3 3 3 4 4 4))))
              (list uniq uniq!))

    (test-group "slow"
      (let ([l (iota 10000000)])
        (time (test-equal l (uniq = l)))
        (time (test-equal l (uniq! = l))))
      (let ([l (iota 100000000)])
        (time (test-equal l (uniq! = l)))))

  (test-end "uniq"))

  (test-begin "uniq-compare")
  (let ()
    (define (random-runs len runmax)
      (let loop ([len len] [l '()])
        (if (<= len 0) l
          (let ([x (random (most-positive-fixnum))]
                [runlen (add1 (random runmax))])
            (let inner ([n runlen] [l l])
              (if (zero? n)
                (loop (- len runlen) l)
                (inner (sub1 n) (cons x l))))))))

    (define (test-compare rpt len runmax)
      (do ([i 0 (add1 i)])
        [(= rpt i)]
        (let* ([l (random-runs len runmax)]
               [u1 (uniq = l)]
               [u2 (uniq! = l)])
          (test-equal u1 u2))))

    (test-group "slow"
      (time (test-compare 2000 200000 500))
      (time (test-compare 100 200000 3))
      (time (test-compare 10 2000000 2))
      (time (test-compare 100 2000000 5000)))
    )
  (test-end "uniq-compare")

  (test-group "any-all"
    (test-assert (any? = 10 '(-1 0 11 10)))
    (test-assert (not (any? = 10 '(1 2 3 9))))
    (test-assert (all? = 10 '(10 10 10 10)))
    (test-assert (not (all? = 10 '(10 10 10 9))))
    (test-assert (not (any-not? = 10 '(10 10 10 10))))
    (test-assert (any-not? = 10 '(10 10 10 9)))
    (test-assert (none? = 10 '(1 2 3 9)))
    (test-assert (not (none? = 10 '(-1 0 11 10))))
    )
  )
