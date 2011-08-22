#!/usr/bin/env gosh

(use srfi-1)
(use gauche.test)
(use util)
(use test.util)

(test-start "color.control.harmonics")

(use color.model.hsl)
(use color.control.harmonics)
(test-module 'color.control.harmonics)

(test-section "tone in tone : hsl")
(define subject-hsl (make <hsl> :hue 0 :saturation 0.8 :luminance 0.6))

(test* "case : num = -1"
       '()
       (tone-in-tone subject-hsl 10 -1))

(test* "case : num = 0"
       '()
       (tone-in-tone subject-hsl 10 0))

(test* "case : num = 1"
       '((0 0.8 0.6))
       (map x->list (tone-in-tone subject-hsl 10 1)))

(test* "case : num = 2"
       '((350 0.8 0.6) (10 0.8 0.6))
       (map x->list (tone-in-tone subject-hsl 10 2)))

(test* "case : num = 3"
       '((350 0.8 0.6) (0 0.8 0.6) (10 0.8 0.6))
       (map x->list (tone-in-tone subject-hsl 10 3)))

(test* "case : num = 4"
       '((340 0.8 0.6) (350 0.8 0.6) (10 0.8 0.6) (20 0.8 0.6))
       (map x->list (tone-in-tone subject-hsl 10 4)))

(test-section "tone in tone : hsla")
(test* "case : num = 3"
       '((350 0.8 0.6 1) (0 0.8 0.6 1) (10 0.8 0.6 1))
       (map x->list
	    (tone-in-tone
	     (make <hsla> :hue 0 :saturation 0.8 :luminance 0.6 :alpha 1) 10 3)))

(test-end)
;; EOF