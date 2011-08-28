#!/usr/bin/env gosh
;; -*- mode: gauche; coding: utf-8; -*-

(define-module color.control.harmonics
  (use gauche.experimental.app)
  (use util)
  (use color.model.hsl)
  (use color.model.hsv)
  (use color.control.convert)
  (export tone-in-tone
	  dyads
	  split-complementary
	  triad))

(select-module color.control.harmonics)

;; make instance function need only hue
(define-method make-from-hue ((origin <hsl>))
  (receive (_ s l) (x->values origin)
    (cut make <hsl> :hue <> :saturation s :luminance l)))

(define-method make-from-hue ((origin <hsv>))
  (receive (_ s v) (x->values origin)
    (cut make <hsv> :hue <> :saturation s :value v)))

;; basic strategy for tone in tone
;; X -> [X]
(define (make-tone-in-tone origin num range)
  (let ([maker (make-from-hue origin)]
	[hue (hue-of origin)]
	[dh (* range (floor (/ num 2)))])
    (cond [(< num 1) '()]
	  [(= num 1) (list origin)]
	  [else `(,(maker (- hue dh))
		  ,@(make-tone-in-tone origin (- num 2) range)
		  ,(maker (+ hue dh)))])))

(define-method tone-in-tone ((base <hsl>) (num <integer>) (range <real>))
  (make-tone-in-tone base num range))

(define-method tone-in-tone ((base <hsv>) (num <integer>) (range <real>))
  (make-tone-in-tone base num range))

;; basic hue-contrast harmony factory function
;; X -> [X]
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
  

(define-method dyads ((base <hsl>)) (hue-contrast base 2))
(define-method dyads ((base <hsv>)) (hue-contrast base 2))
(define-method triad ((base <hsl>)) (hue-contrast base 3))
(define-method triad ((base <hsv>)) (hue-contrast base 3))

(define-method split-complementary ((base <hsl>))
  (let1 compl (car (dyads base))
    (cons base (tone-in-tone compl 2 15))))

(define-method split-complementary ((base <hsv>))
  (let1 compl (car (dyads base))
    (cons base (tone-in-tone compl 2 15))))

;; XA -> [XA]
(define (make-tone/alpha proc origin . needs)
  (let ([without-alpha-object (remove-alpha origin)]
	[alpha (alpha-of origin)])
    (map (cut add-alpha <> alpha)
	 (apply proc without-alpha-object needs))))

(define-method tone-in-tone ((base <hsla>) (num <integer>) (range <real>))
  (make-tone/alpha tone-in-tone base num range))

(define-method tone-in-tone ((base <hsva>) (num <integer>) (range <real>))
  (make-tone/alpha tone-in-tone base num range))

(define-method dyads ((base <hsla>))
 (make-tone/alpha dyads base))

(define-method dyads ((base <hsva>))
 (make-tone/alpha dyads base))

(define-method split-complementary ((base <hsla>))
 (make-tone/alpha split-complementary base))

(define-method split-complementary ((base <hsva>))
 (make-tone/alpha split-complementary base))

(define-method triad ((base <hsla>)) (make-tone/alpha triad base))
(define-method triad ((base <hsva>)) (make-tone/alpha triad base))

(provide "color/control/harmonics")