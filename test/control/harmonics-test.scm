#!/usr/bin/env gosh

(use srfi-1)
(use gauche.test)
(use util)
(use test.util)

(test-start "color.control.harmonics")

(use color.model)
(use color.control.harmonics)
(test-module 'color.control.harmonics)

(define subject-hsl (make <hsl> :hue 0 :saturation 0.8 :luminance 0.6))
(define subject-hsla (make <hsla> :hue 0 :saturation 0.8 :luminance 0.6 :alpha 1))
(define subject-hsv (make <hsv> :hue 0 :saturation 0.8 :value 0.7))
(define subject-hsva (make <hsva> :hue 0 :saturation 0.8 :value 0.7 :alpha 1))

(test-section "tone in tone : hsl")

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

(test-section "tone in tone : hsla")
(test* "case : num = 3" '((350 0.8 0.6 1) (0 0.8 0.6 1) (10 0.8 0.6 1))
       (map x->list (tone-in-tone subject-hsla 3 10)))

(test-section "tone in tone : hsva")
(test* "case : num = 3" '((350 0.8 0.7 1) (0 0.8 0.7 1) (10 0.8 0.7 1))
       (map x->list (tone-in-tone  subject-hsva 3 10)))

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

(test-end)
;; EOF