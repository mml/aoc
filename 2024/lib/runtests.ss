#!/usr/bin/env -S scheme --program
(import (chezscheme) (srfi :64))
; See https://srfi.schemers.org/srfi-64/srfi-64.html

(test-runner-current (test-runner-simple))
(include "convolve-test.ss")
(test-runner-current (test-runner-simple))
(include "for-test.ss")
(test-runner-current (test-runner-simple))
(include "util-test.ss")
