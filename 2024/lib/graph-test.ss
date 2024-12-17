(import (graph))

(define (doit)
  (define q (make-prioq))
  (prioq-add! q 'foo 99)
  (printf "~a\n" q)
  (prioq-add! q 'bar 12)
  (printf "~a\n" q)
  (prioq-add! q 'baz 100)
  (printf "~a\n" q)

  (decrease! q 'bar 11)
  (printf "~a\n" q)

  (decrease! q 'foo 90)
  (printf "~a\n" q)

  (decrease! q 'baz 85)
  (printf "~a\n" q)

  (prioq-add! q 'quux 80)
  (printf "~a\n" q)

  (decrease! q 'foo 77)
  (printf "~a\n" q)
  )

(doit)
