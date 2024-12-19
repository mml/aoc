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

  (test-begin "map-with-values")
  (test-runner-on-test-end! (test-runner-get) test-on-test-end-diff)
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

  (test-end "map-with-values"))
