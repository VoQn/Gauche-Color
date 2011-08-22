#!/usr/bin/env gosh

;(add-load-path "/Users/VoQn/tmp/gauche-color")

(define-module color.control.harmonics
  (use util)
  (use color.model.hsl)
  (use color.model.hsv)
  (use color.control.convert)
  (export-all))

(select-module color.control.harmonics)

(define-method tone-in-tone ((base <hsl>)
			     (range <real>)
			     (num <integer>))
  (receive (h s l) (x->values base)
    (cond [(< num 1) '()]
	  [(= num 1) (list base)]
	  [(= num 2)
	   (list (make <hsl> :hue (- h range) :saturation s :luminance l)
		 (make <hsl> :hue (+ h range) :saturation s :luminance l))]
	  [else `(,(make <hsl>
		     :hue (- h (* range (floor (/ num 2))))
		     :saturation s :luminance l)
		  ,@(tone-in-tone base range (- num  2))
		  ,(make <hsl>
		     :hue (+ h (* range (floor (/ num 2))))
		     :saturation s :luminance l))])))

(define-method tone-in-tone ((base <hsla>)
			     (range <real>)
			     (num <integer>))
  (let1 a (alpha-of base)
    (map (cut add-alpha <> a)
	 (tone-in-tone (remove-alpha base) range num))))

(define-method tone-in-tone ((base <hsv>)
			     (range <real>)
			     (num <integer>))
  (receive (h s v) (x->values base)
    (cond [(< num 1) '()]
	  [(= num 1) (list base)]
	  [(= num 2)
	   (list (make <hsv> :hue (- h range) :saturation s :value v)
		 (make <hsv> :hue (+ h range) :saturation s :value v))]
	  [else `(,(make <hsl>
		     :hue (- h (* range (floor (/ num 2))))
		     :saturation s :value v)
		  ,@(tone-in-tone base range (- num  2))
		  ,(make <hsl>
		     :hue (+ h (* range (floor (/ num 2))))
		     :saturation s :value v))])))

(define-method tone-in-tone ((base <hsva>)
			     (range <real>)
			     (num <integer>))
  (let1 a (alpha-of base)
    (map (cut add-alpha <> a)
	 (tone-in-tone (remove-alpha base) range num))))

(provide "color/control/harmonics")