#!/usr/bin/env gosh

(use gauche.test)
(use util)
(use test.util)

(test-start "color.model.rgb")

(use color.model.rgb)
(test-module 'color.model.rgb)

;; testing each value between expect and result float list
(define enough? (tolerance$ 1e-5))

;;; RGB Color Space Model
(test-section "RGB Color Model")
(test-type* "make rgb color object" <rgb> (make <rgb>))

(test-list* "convert to list" '(0.1 0.2 0.3)
	    (make <rgb> :red 0.1 :green 0.2 :blue 0.3) enough?)

(test-list* "add brightness" '(0.2 0.3 0.4)
	    ((cut +bright <> 0.1)
	     (make <rgb> :red 0.1 :green 0.2 :blue 0.3)) enough?)

(test* "hex expression" "#ffccff"
       (hex-expr (make <rgb> :red 1 :green 0.8 :blue 1.0)))

(test* "css string" "rgb(100%, 30%, 40%)"
       (x->string (make <rgb> :red 1 :green 0.3 :blue 0.4)))

;; RGBA
(test-section "RGBA Color Model")
(test-type* "make rgba color object" <rgba> (make <rgba>))

(test-list* "convert to list" '(0.1 0.2 0.3 0.4)
	    (make <rgba> :red 0.1 :green 0.2 :blue 0.3 :alpha 0.4) enough?)

(test-list* "add brightness" '(0.2 0.3 0.4 0.4)
	    ((cut +bright <> 0.1)
	     (make <rgba> :red 0.1 :green 0.2 :blue 0.3 :alpha 0.4)) enough?)

(test* "hex expression" "#00ff00ff"
       (hex-expr (make <rgba> :red 0 :green 1 :blue 0 :alpha 1.0)))

(test* "css string" "rgba(100%, 50%, 30%, 1.0)"
       (x->string (make <rgba> :red 1 :green 0.5 :blue 0.3 :alpha 1.0)))
(test-section "Add & Remove Alpha Channel")

(test-type* "convert rgb -> rgba" <rgba> ((cut add-alpha <> 0.5) (make <rgb>)))
(test-type* "convert rgba -> rgb" <rgb> (remove-alpha (make <rgba>)))

(test-end)