#!/usr/bin/env gosh

(use gauche.test)
(use util)
(use test.util)

(test-start "color.model.hcx")

(use color.model.hcx)
(test-module 'color.model.hcx)

;; testing each value between expect and result float list
(define enough? (tolerance 1e-5))

;;; HCX
(test-section "HCX Color Model")
(test-type* "make hcx color object" <hcx> (make <hcx>))

(test-list* "convert to list" '(0.1 0.2 0.3)
	    (make <hcx> :hue 0.1 :chroma 0.2 :x 0.3)
	    enough?)

(test-end)
;; EOF