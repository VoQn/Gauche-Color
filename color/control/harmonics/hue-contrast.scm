#!/usr/bin/env gosh
;; -*- mode: gauche; coding: utf-8; -*-

(define-module color.control.harmonics.hue-contrast
  (use util)
  (use color.model)
  (use color.control.convert)
  (export tone-in-tone
	  dyads
	  split-complementary
	  triad
	  tetrad
	  pentads
	  hexads))

(select-module color.control.harmonics.hue-contrast)

;; make instance function need only hue
(define-method make-from-hue ((origin <hsl>))
  (receive (_ s l) (x->values origin)
    (cut make <hsl> :hue <> :saturation s :luminance l)))

(define-method make-from-hue ((origin <hsla>))
  (receive (_ s l a) (x->values origin)
    (cut make <hsla> :hue <> :saturation s :luminance l :alpha a)))

(define-method make-from-hue ((origin <hsv>))
  (receive (_ s v) (x->values origin)
    (cut make <hsv> :hue <> :saturation s :value v)))

(define-method make-from-hue ((origin <hsva>))
  (receive (_ s v a) (x->values origin)
    (cut make <hsva> :hue <> :saturation s :value v :alpha a)))

(define-method make-from-hue ((origin <rgb>))
  (receive (_ s l) (x->values (rgb->hsl origin))
    (^h (x->rgb (make <hsl> :hue h :saturation s :luminance l)))))

(define-method make-from-hue ((origin <rgba>))
  (receive (_ s l a) (x->values (rgba->hsla origin))
    (^h (x->rgba (make <hsla> :hue h :saturation s :luminance l :alpha a)))))

(define-method hue-of ((rgb <rgb>))
  (hue-of (rgb->hsl rgb)))

(define-method hue-of ((rgba <rgba>))
  (hue-of (rgba->hsla rgba)))

;; basic strategy for tone in tone
;; Integer -> Float -> Color -> [Color]
(define (make-tone-in-tone origin num range)
  (let ([maker (make-from-hue origin)]
	[hue (hue-of origin)])
    (let iter ([count num])
      (let1 dh (* range (floor (/ count 2)))
	(cond [(< count 1) '()]
	      [(= count 1) (list origin)]
	      [else `(,(maker (- hue dh))
		      ,@(iter (- count 2))
		      ,(maker (+ hue dh)))])))))

(define-method tone-in-tone ((base <color>) (num <integer>) (range <real>))
  (make-tone-in-tone base num range))

;; basic hue-contrast harmony factory function
;; Int -> Color -> [Color]
(define (hue-contrast origin split-count)
  (let ([maker (make-from-hue origin)]
	[dh (/. 360 split-count)])
    (let iter ([result (list origin)]
	       [hue (hue-of origin)]
	       [count split-count])
      (cond [(< count 1) '()]
	    [(= count 1) result]
	    [else (iter (cons (maker (+ hue dh)) result)
			(+ hue dh)
			(- count 1))]))))

(define-method dyads   ((base <color>)) (hue-contrast base 2))
(define-method triad   ((base <color>)) (hue-contrast base 3))
(define-method tetrad  ((base <color>)) (hue-contrast base 4))
(define-method pentads ((base <color>)) (hue-contrast base 5))
(define-method hexads  ((base <color>)) (hue-contrast base 6))

(define (make-split-compl o)
  (let1 compl (car (dyads o)) (cons o (tone-in-tone compl 2 15))))

(define-method split-complementary ((base <color>)) (make-split-compl base))

(provide "color/control/harmonics/hue-contrast")
;; EOF
