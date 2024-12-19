#!/usr/bin/env -S scheme --program
(import (chezscheme) (srfi :26) (util)); (graph) (gridvector) (product))
(define (read-file f)
  (with-input-from-file f
    (lambda ()
      ; loop probably based on one of
      ; (let loop ([line (get-line (current-input-port))] [lines '()])
      ; (let loop ([datum (read)] [acc '()])
      ; (let loop ([ch (read-char)] [acc '()])
      ;
      ;   (cond
      ;     [(eof-object? XXX) (reverse! YYY)]
      ;     [--- ---]
      ;     [else ---])
      ;   --- )
      ---)))

; (define (make-foo f)
;   (let ([--- (read-file f)])
;     ---))

(define (main f)
  (time
    ;(let ([gv (gridvector-from-file f)])
    (let ([lines (get-lines-from-file f)])
      ---)))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (apply main args)))
