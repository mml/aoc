#!/usr/bin/env -S scheme --program
(import (chezscheme) (util)); (gridvector) (product))
(define (read-file f)
  (with-input-from-file f
    (lambda ()
      #|
      loop probably based on one of
      (get-line (current-input-port))
      (read)
      (read-char)
      |#
      ---)))
#|
(define (make-foo f)
  (let ([--- (read-file f)])
    ---))
|#
(define (main f)
  (time
    ;(let ([gv (gridvector-from-file f)])
    (let ([lines (get-lines-from-file f)])
      ---)))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
