#!/usr/bin/env gosh

(use gauche.test)
(test-record-file "./test/temp/test.record")
(set! *test-report-error* #t)

;; EOF
