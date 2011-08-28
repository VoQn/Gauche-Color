#!/usr/bin/env gosh

(use gauche.test)
(use util)
(use test.util)

(test-start "color.control.convert")

(use color.model)
(use color.control.convert)
(test-module 'color.control.convert)

;;; Convert
(test-section "HCX -> RGB")
(test-type* "convert type" <rgb> (x->rgb (make <hcx>)))

(test-list* "case (0 <= hue < 1)" '(0.5 1.0 0)
	    (x->rgb (make <hcx> :hue 0.0
			  :chroma 0.5 :x 1)) enough?)

(test-list* "case (1 <= hue < 2)" '(1.0 0.5 0)
	    (x->rgb (make <hcx> :hue 1.0
			  :chroma 0.5 :x 1)) enough?)

(test-list* "case (2 <= hue < 3)" '(0 0.5 1.0)
	    (x->rgb (make <hcx> :hue 2.0
			  :chroma 0.5 :x 1)) enough?)

(test-list* "case (3 <= hue < 4)" '(0 1.0 0.5)
	    (x->rgb (make <hcx> :hue 3.0
			  :chroma 0.5 :x 1)) enough?)

(test-list* "case (4 <= hue < 5)" '(1.0 0 0.5)
	    (x->rgb (make <hcx> :hue 4.0
			  :chroma 0.5 :x 1)) enough?)

(test-list* "case (5 <= hue < 6)" '(0.5 0 1.0)
	    (x->rgb (make <hcx> :hue 5.0
			  :chroma 0.5 :x 1)) enough?)

(test-section "HSL -> RGB")
(test-type* "convert type" <rgb> (x->rgb (make <hsl>)))

(test-list* "case hue = 0 (0 <= hue < 60)" '(1 0.3 0.3)
	    (x->rgb (make <hsl> :hue 0
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 30 (0 <= hue < 60)" '(1 0.65 0.3)
	    (x->rgb (make <hsl> :hue 30
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 60 (60 <= hue < 120)" '(1 1 0.3)
	    (x->rgb (make <hsl> :hue 60
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 90 (60 <= hue < 120)" '(0.65 1 0.3)
	    (x->rgb (make <hsl> :hue 90
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 120 (120 <= hue < 180)" '(0.3 1 0.3)
	    (x->rgb (make <hsl> :hue 120
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 150 (120 <= hue < 180)" '(0.3 1 0.65)
	    (x->rgb (make <hsl> :hue 150
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 180 (180 <= hue < 240)" '(0.3 1 1)
	    (x->rgb (make <hsl> :hue 180
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 210 (180 <= hue < 240)" '(0.3 0.65 1)
	    (x->rgb (make <hsl> :hue 210
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 240 (240 <= hue < 300)" '(0.3 0.3 1)
	    (x->rgb (make <hsl> :hue 240
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 270 (240 <= hue < 300)" '(0.65 0.3 1)
	    (x->rgb (make <hsl> :hue 270
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 300 (300 <= hue < 360)" '(1 0.3 1)
	    (x->rgb (make <hsl> :hue 300
			  :saturation 1 :luminance 0.65)) enough?)

(test-list* "case hue = 330 (300 <= hue < 360)" '(1 0.3 0.65)
	    (x->rgb (make <hsl> :hue 330
			  :saturation 1 :luminance 0.65)) enough?)

(test-section "HSLA -> RGBA")
(test-type* "convert type" <rgba> (x->rgba (make <hsla>)))

(test-list* "case hue = 330 (300 <= hue < 360)" '(1 0.3 0.65 0.5)
	    (x->rgba (make <hsla> :hue 330
			  :saturation 1 :luminance 0.65 :alpha 0.5)) enough?)

(test-section "HSV -> RGB")
(test-type* "convert type" <rgb> (x->rgb (make <hsv>)))

(test-list* "case saturation is 0" '(1 1 1)
	    (x->rgb (make <hsv> :hue 0 :saturation 0
			  :value 1)) enough?)

(test-list* "case hue = 0 (0 <= hue < 60)" '(1 0 0)
	    (x->rgb (make <hsv> :hue 0
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 30 (0 <= hue < 60)" '(1 0.5 0)
	    (x->rgb (make <hsv> :hue 30
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 60 (60 <= hue < 120)" '(1 1 0)
	    (x->rgb (make <hsv> :hue 60
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 90 (60 <= hue < 120)" '(0.5 1 0)
	    (x->rgb (make <hsv> :hue 90
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 120 (120 <= hue < 180)" '(0 1 0)
	    (x->rgb (make <hsv> :hue 120
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 150 (120 <= hue < 180)" '(0 1 0.5)
	    (x->rgb (make <hsv> :hue 150
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 180 (180 <= hue < 240)" '(0 1 1)
	    (x->rgb (make <hsv> :hue 180
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 210 (180 <= hue < 240)" '(0 0.5 1)
	    (x->rgb (make <hsv> :hue 210
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 240 (240 <= hue < 300)" '(0 0 1)
	    (x->rgb (make <hsv> :hue 240
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 270 (240 <= hue < 300)" '(0.5 0 1)
	    (x->rgb (make <hsv> :hue 270
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 300 (300 <= hue < 360)" '(1 0 1)
	    (x->rgb (make <hsv> :hue 300
			  :saturation 1 :value 1)) enough?)

(test-list* "case hue = 330 (300 <= hue < 360)" '(1 0 0.5)
	    (x->rgb (make <hsv> :hue 330
			  :saturation 1 :value 1)) enough?)


(test-section "RGB -> HSL")
(test-type* "convert type" <hsl> (rgb->hsl (make <rgb>)))

(test-list* "case each equal value (0.5 0.5 0.5)" '(0 0 0.5)
	    (rgb->hsl (+bright (make <rgb>) 0.5)) enough?)

(test-list* "case max value is red" '(350 1 0.7)
	    (rgb->hsl (make <rgb> :red 1.0 :green 0.4 :blue 0.5)) enough?)

(test-list* "case max value is green" '(130 1 0.7)
	    (rgb->hsl (make <rgb> :red 0.4 :green 1.0 :blue 0.5)) enough?)

(test-list* "case max value is blue" '(250 1 0.7)
	    (rgb->hsl (make <rgb> :red 0.5 :green 0.4 :blue 1.0)) enough?)

(test-section "RGBA -> HSLA")
(test-type* "convert type" <hsla> (rgba->hsla (make <rgba>)))

(test-list* "case each equal value (0.5 0.5 0.5 1.0)" '(0 0 0.5 1.0)
	    (rgba->hsla (+bright (make <rgba> :alpha 1.0) 0.5)) enough?)

(test-end)
;; EOF