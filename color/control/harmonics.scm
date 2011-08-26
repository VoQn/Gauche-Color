#!/usr/bin/env gosh
;; -*- mode: gauche; coding: utf-8; -*-

(define-module color.control.harmonics
  (use gauche.experimental.app)
  (use util)
  (use color.model.hsl)
  (use color.model.hsv)
  (use color.control.convert)
  (export-all))

(select-module color.control.harmonics)

;; basic storategy for tone in tone
(define (make-tone-in-tone o m n r)
  (let ([h (hue-of o)]
	[dh (* r (floor (/ n 2)))])
    (cond [(< n 1) '()]
	  [(= n 1) (list o)]
	  [else `(,(m (- h dh))
		  ,@(make-tone-in-tone o m (- n 2) r)
		  ,(m (+ h dh)))])))

(define-method tone-in-tone ((base <hsl>) (num <integer>) (range <real>))
  (receive (_ s l) (x->values base)
    (let1 make-hsl (cut make <hsl> :hue <> :saturation s :luminance l)
      (make-tone-in-tone base make-hsl num range))))

(define-method tone-in-tone ((base <hsv>) (num <integer>) (range <real>))
  (receive (_ s v) (x->values base)
    (let1 make-hsv (cut make <hsv> :hue <> :saturation s :value v)
      (make-tone-in-tone base make-hsv num range))))

(define (hue-contrast color-has-hue make-proc split-count . opt-range)
  (let1 range (get-optional opt-range (/ 360 split-count))
    (let iter ([result '()]
	       [hue (hue-of color-has-hue)]
	       [dh range]
	       [count split-count])
      (cond [(< count 1) result]
	    [(= count 1) color-has-hue]
	    [else (iter (cons (make-proc (+ hue dh)) result)
			(+ hue dh)
			dh
			(- count 1))]))))
	       
(define-method dyads ((base <hsl>)) (hue-contrast base 2))
(define-method dyads ((base <hsv>)) (hue-contrast base 2))
(define-method triad ((base <hsl>)) (hue-contrast base 3))
(define-method triad ((base <hsv>)) (hue-contrast base 3))

(define (make-tone/alpha obj proc . needs)
  (let1 a (alpha-of obj)
    (map (cut add-alpha <> a)
	 (apply proc (remove-alpha obj) needs))))

(define-method tone-in-tone ((base <hsla>) (num <integer>) (range <real>))
  (make-tone/alpha base tone-in-tone num range))

(define-method tone-in-tone ((base <hsva>) (num <integer>) (range <real>))
  (make-tone/alpha base tone-in-tone num range))

(define-method dyads ((base <hsla>)) (hue-contrast base 2))
(define-method dyads ((base <hsva>)) (hue-contrast base 2))
(define-method triad ((base <hsla>)) (hue-contrast base 3))
(define-method triad ((base <hsva>)) (hue-contrast base 3))

(provide "color/control/harmonics")