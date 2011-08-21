#!/usr/bin/env gosh

(use gauche.test)
(use util)
(use test.util)

(test-start "color.model.hsl")

(use color.model.hsl)
(test-module 'color.model.hsl)

;; testing each value between expect and result float list
(define enough? (tolerance$ 1e-5))

;; HSL
(test-section "HSL Color Model")
(test-type* "make hsl color object" <hsl> (make <hsl>))

(test-list* "convert to list" '(100 0.2 0.3)
	    (make <hsl>
	      :hue 100
	      :saturation 0.2
	      :luminance 0.3)
	    enough?)

(test* "css string" "hsl(300, 80%, 50%)"
       (x->string (make <hsl>
		    :hue 300
		    :saturation 0.8
		    :luminance 0.5)))

;; HSLA
(test-section "HSLA Color Model")
(test-type* "make hsla color object" <hsla> (make <hsla>))

(test-list* "convert to list" '(100 0.2 0.3 0.4)
	    (make <hsla>
	      :hue 100
	      :saturation 0.2
	      :luminance 0.3
	      :alpha 0.4)
	    enough?)

(test* "css string" "hsla(300, 80%, 50%, 1.0)"
       (x->string 
	(make <hsla>
	  :hue 300
	  :saturation 0.8
	  :luminance 0.5
	  :alpha 1.0)))

(test-section "Add & Remove Alpha Channel")

(test-type* "convert hsl -> hsla" <hsla>
	    ((cut add-alpha <> 0.5) (make <hsl>)))

(test-type* "convert hsla -> hsl" <hsl>
	    (remove-alpha (make <hsla>)))

(test-end)
;; EOF