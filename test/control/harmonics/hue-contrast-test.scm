#!/usr/bin/env gosh

(use srfi-1)
(use gauche.test)
(use util)
(use test.util)

(test-start "color.control.harmonics.hue-contrast")

(use color.model)
(use color.control.harmonics.hue-contrast)
(test-module 'color.control.harmonics.hue-contrast)


(test-section "tone in tone : hsl")

(define subject-hsl
  (make <hsl> :hue 0 :saturation 0.8 :luminance 0.6))

(test* "case : num = -1" '()
       (tone-in-tone subject-hsl -1 10))

(test* "case : num = 0" '()
       (tone-in-tone subject-hsl 0 10))

(test* "case : num = 1" '((0 0.8 0.6))
       (map x->list (tone-in-tone subject-hsl 1 10)))

(test* "case : num = 2" '((350 0.8 0.6) (10 0.8 0.6))
       (map x->list (tone-in-tone subject-hsl 2 10)))

(test* "case : num = 3" '((350 0.8 0.6) (0 0.8 0.6) (10 0.8 0.6))
       (map x->list (tone-in-tone subject-hsl 3 10)))

(test* "case : num = 4" '((340 0.8 0.6) (350 0.8 0.6) (10 0.8 0.6) (20 0.8 0.6))
       (map x->list (tone-in-tone subject-hsl 4 10)))

(test* "case : num = 5" '((340 0.8 0.6) (350 0.8 0.6) (0 0.8 0.6) (10 0.8 0.6) (20 0.8 0.6))
       (map x->list (tone-in-tone subject-hsl 5 10)))


(test-section "tone in tone : hsv")

(define subject-hsv
  (make <hsv> :hue 0 :saturation 0.8 :value 0.7))

(test* "case : num = -1" '()
       (tone-in-tone subject-hsv -1 10))

(test* "case : num = 0" '()
       (tone-in-tone subject-hsv 0 10))

(test* "case : num = 1" '((0 0.8 0.7))
       (map x->list (tone-in-tone subject-hsv 1 10)))

(test* "case : num = 2" '((350 0.8 0.7) (10 0.8 0.7))
       (map x->list (tone-in-tone subject-hsv 2 10)))

(test* "case : num = 3" '((350 0.8 0.7) (0 0.8 0.7) (10 0.8 0.7))
       (map x->list (tone-in-tone subject-hsv 3 10)))

(test* "case : num = 4" '((340 0.8 0.7) (350 0.8 0.7) (10 0.8 0.7) (20 0.8 0.7))
       (map x->list (tone-in-tone subject-hsv 4 10)))

(test* "case : num = 5" '((340 0.8 0.7) (350 0.8 0.7) (0 0.8 0.7) (10 0.8 0.7) (20 0.8 0.7))
       (map x->list (tone-in-tone subject-hsv 5 10)))


(test-section "tone in tone : rgb")

;; (hex-expr subject-rgb) ; => #ffcc33
(define subject-rgb
  (make <rgb> :red 1.0 :green 0.8 :blue 0.201))

(test* "case : num = -1" '()
       (tone-in-tone subject-rgb -1 10))

(test* "case : num = 0" '()
       (tone-in-tone subject-rgb 0 10))

(test* "case : num = 1" '("#ffcc33")
       (map hex-expr (tone-in-tone subject-rgb 1 10)))

(test* "case : num = 2" '("#ffaa33" "#ffed33")
       (map hex-expr (tone-in-tone subject-rgb 2 10)))

(test* "case : num = 3" '("#ffaa33" "#ffcc33" "#ffed33")
       (map hex-expr (tone-in-tone subject-rgb 3 10)))

(test* "case : num = 4" '("#ff8833" "#ffaa33" "#ffed33" "#eeff33")
       (map hex-expr (tone-in-tone subject-rgb 4 10)))

(test* "case : num = 5" '("#ff8833" "#ffaa33" "#ffcc33" "#ffed33" "#eeff33")
       (map hex-expr (tone-in-tone subject-rgb 5 10)))


(test-section "tone in tone : hsla")

(define subject-hsla
  (make <hsla> :hue 0 :saturation 0.8 :luminance 0.6 :alpha 1))

(test* "case : num = 3" '((350 0.8 0.6 1) (0 0.8 0.6 1) (10 0.8 0.6 1))
       (map x->list (tone-in-tone subject-hsla 3 10)))


(test-section "tone in tone : hsva")

(define subject-hsva
  (make <hsva> :hue 0 :saturation 0.8 :value 0.7 :alpha 1))

(test* "case : num = 3" '((350 0.8 0.7 1) (0 0.8 0.7 1) (10 0.8 0.7 1))
       (map x->list (tone-in-tone  subject-hsva 3 10)))


(test-section "tone in tone : rgba")

;; (hex-expr subject-rgba) ; => #ffcc33ff
(define subject-rgba
  (make <rgba> :red 1.0 :green 0.8 :blue 0.201 :alpha 1))

(test* "case : num = 3" '("#ffaa33ff" "#ffcc33ff" "#ffed33ff")
       (map hex-expr (tone-in-tone subject-rgba 3 10)))

(test-section "dyads")
(test* "hsl model" '((180 0.8 0.6) (0 0.8 0.6))
       (map x->list (dyads subject-hsl)))

(test* "hsla model" '((180 0.8 0.6 1) (0 0.8 0.6 1))
       (map x->list (dyads subject-hsla)))

(test* "hsv model" '((180 0.8 0.7) (0 0.8 0.7))
       (map x->list (dyads subject-hsv)))

(test* "hsva model" '((180 0.8 0.7 1) (0 0.8 0.7 1))
       (map x->list (dyads subject-hsva)))

(test-section "split-complementary")
(test* "hsl model" '((0 0.8 0.6) (165 0.8 0.6) (195 0.8 0.6))
       (map x->list (split-complementary subject-hsl)))

(test* "hsla model" '((0 0.8 0.6 1) (165 0.8 0.6 1) (195 0.8 0.6 1))
       (map x->list (split-complementary subject-hsla)))

(test* "hsv model" '((0 0.8 0.7) (165 0.8 0.7) (195 0.8 0.7))
       (map x->list (split-complementary subject-hsv)))

(test* "hsva model" '((0 0.8 0.7 1) (165 0.8 0.7 1) (195 0.8 0.7 1))
       (map x->list (split-complementary subject-hsva)))

(test-section "triad")
(test* "hsl model" '((240 0.8 0.6) (120 0.8 0.6) (0 0.8 0.6))
       (map x->list (triad subject-hsl)))

(test* "hsla model" '((240 0.8 0.6 1) (120 0.8 0.6 1) (0 0.8 0.6 1))
       (map x->list (triad subject-hsla)))

(test* "hsv model" '((240 0.8 0.7) (120 0.8 0.7) (0 0.8 0.7))
       (map x->list (triad subject-hsv)))

(test* "hsva model" '((240 0.8 0.7 1) (120 0.8 0.7 1) (0 0.8 0.7 1))
       (map x->list (triad subject-hsva)))

(test-section "tetrad")
(test* "hsl model" '((270 0.8 0.6)
		     (180 0.8 0.6)
		     (90 0.8 0.6)
		     (0 0.8 0.6))
       (map x->list (tetrad subject-hsl)))

(test* "hsla model" '((270 0.8 0.6 1)
		      (180 0.8 0.6 1)
		      (90 0.8 0.6 1)
		      (0 0.8 0.6 1))
       (map x->list (tetrad subject-hsla)))

(test* "hsv model" '((270 0.8 0.7)
		     (180 0.8 0.7)
		     (90 0.8 0.7)
		     (0 0.8 0.7))
       (map x->list (tetrad subject-hsv)))

(test* "hsva model" '((270 0.8 0.7 1)
		      (180 0.8 0.7 1)
		      (90 0.8 0.7 1)
		      (0 0.8 0.7 1))
       (map x->list (tetrad subject-hsva)))

(test-section "pentads")
(test* "hsl model" '((288 0.8 0.6)
		     (216 0.8 0.6)
		     (144 0.8 0.6)
		     (72 0.8 0.6)
		     (0 0.8 0.6))
       (map x->list (pentads subject-hsl)))

(test* "hsla model" '((288 0.8 0.6 1)
		      (216 0.8 0.6 1)
		      (144 0.8 0.6 1)
		      (72 0.8 0.6 1)
		      (0 0.8 0.6 1))
       (map x->list (pentads subject-hsla)))

(test* "hsv model" '((288 0.8 0.7)
		     (216 0.8 0.7)
		     (144 0.8 0.7)
		     (72 0.8 0.7)
		     (0 0.8 0.7))
       (map x->list (pentads subject-hsv)))

(test* "hsva model" '((288 0.8 0.7 1)
		      (216 0.8 0.7 1)
		      (144 0.8 0.7 1)
		      (72 0.8 0.7 1)
		      (0 0.8 0.7 1))
       (map x->list (pentads subject-hsva)))

(test-section "hexads")
(test* "hsl model" '((300 0.8 0.6)
		     (240 0.8 0.6)
		     (180 0.8 0.6)
		     (120 0.8 0.6)
		     (60 0.8 0.6)
		     (0 0.8 0.6))
       (map x->list (hexads subject-hsl)))

(test* "hsla model" '((300 0.8 0.6 1)
		      (240 0.8 0.6 1)
		      (180 0.8 0.6 1)
		      (120 0.8 0.6 1)
		      (60 0.8 0.6 1)
		      (0 0.8 0.6 1))
       (map x->list (hexads subject-hsla)))

(test* "hsv model" '((300 0.8 0.7)
		     (240 0.8 0.7)
		     (180 0.8 0.7)
		     (120 0.8 0.7)
		     (60 0.8 0.7)
		     (0 0.8 0.7))
       (map x->list (hexads subject-hsv)))

(test* "hsva model" '((300 0.8 0.7 1)
		      (240 0.8 0.7 1)
		      (180 0.8 0.7 1)
		      (120 0.8 0.7 1)
		      (60 0.8 0.7 1)
		      (0 0.8 0.7 1))
       (map x->list (hexads subject-hsva)))

(test-end)
;; EOF