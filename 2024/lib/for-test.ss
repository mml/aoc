(import (chezscheme) (for))

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

  (test-begin "for-test")
  (test-runner-on-test-end! (test-runner-get) test-on-test-end-diff)
  (test-equal 55
              (let ([sum 0])
                (for ([i (iota 11)])
                  (set! sum (+ sum i)))
                sum))
  (test-equal 55
              (for/fold ([sum 0])
                        ([i (iota 11)])
                (+ sum i)))
  (let ([x #f])
    (test-equal 110
      (for/fold ([sum 0])
                (result (begin
                          (set! x sum)
                          (* 2 sum)))
                ([i (iota 11)])
                (+ sum i)))
    (test-equal 55 x))
  (test-equal 110
              (for/fold ([sum 0])
                        (result (* 2 sum))
                        ([i (iota 11)])
                (+ sum i)))
  (test-equal '(a2 b2 a2 b1 a1 b2 a1 b1)
              (let ([rv '()])
                (for* ([a '(a1 a2)]
                       [b '(b1 b2)])
                  (set! rv (cons* a b rv)))
                rv))


  (test-equal (* 11 22 33)
              (let ([data (lambda () (values '(1 2 3) '(10 20 30)))]
                    [prod 1])
                (for ([(a b) (data)])
                  (set! prod (* prod (+ a b))))
                prod))

  (test-equal 300
              (let ([data (lambda () (values '(20 30 40) '(30 40 50)))]
                    [sum 0])
                (for ([(a b) (data)]
                      [c '(50 30 10)])
                  (set! sum (+ sum a b c)))
                sum))

  (test-equal 300
              (let ([data (lambda () (values '(20 30 40) '(30 40 50)))]
                    [sum 0])
                (for ([c '(50 30 10)]
                      [(a b) (data)])
                  (set! sum (+ sum a b c)))
                sum))

  (test-end "for-test"))
#|
(for/fold ([sum 0]
           [rev-roots '()])
          (result (printf "~a~%~a~%" sum rev-roots))
          ([i '(1 2 3 4)])
  (when (> i 2) (break))
  (values (+ sum i) (cons (sqrt i) rev-roots)))

(printf "---~%")
|#

