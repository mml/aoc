(import (chezscheme) (graph))

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
  (test-group "make-node"
    (let ([v (make-node "foo")])
      (test-equal "foo" (node-name v))))
  (test-group "props"
    (let ([v (make-node "foo")])
      (node-set-prop! v 'baz 'quux)
      (test-equal 'quux (node-prop-value v 'baz))
      (test-equal 'quux (cdr (node-propq v 'baz)))
      (test-equal #f (node-propq v 'nope))
      (node-set-prop! v 'baz 'quuux)
      (test-equal 'quuux (node-prop-value v 'baz))))
  )
