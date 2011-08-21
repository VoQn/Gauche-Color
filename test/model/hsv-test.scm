#!/usr/bin/env gosh

(use gauche.test)
(use util)
(use test.util)

(test-start "color.model.hsv")

(use color.model.hsv)
(test-module 'color.model.hsv)

;; testing each value between expect and result float list
(define enough? (tolerance$ 1e-5))

(test-section "HSV Color Model")

(test-type* "make hsv color object" <hsv> (make <hsv>))

(test-list* "convert to list" '(300 1.0 0.5)
	    (make <hsv>
	      :hue 300
	      :saturation 1.0
	      :value 0.5)
	    enough?)

(test-section "HSVA Color Model")

(test-type* "make hsva color object" <hsva> (make <hsva>))

(test-list* "convert to list" '(300 1.0 0.5 0.8)
	    (make <hsva>
	      :hue 300
	      :saturation 1.0
	      :value 0.5
	      :alpha 0.8)
	    enough?)

(test-section "Add & Remove Alpha Channel")

(test-type* "convert hsv -> hsva" <hsva>
	    ((cut add-alpha <> 0.5) (make <hsv>)))

(test-type* "convert hsva -> hsv" <hsv>
	    (remove-alpha (make <hsva>)))

(test-end)
;; EOF
