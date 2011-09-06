#!/usr/bin/env gosh
;; -*- mode: gauche; coding: utf-8; -*-

(define-module color.model.rgb
  (use util)
  (use srfi-11)
  (extend color.model.meta)
  (export <rgb>
	  <rgba>
	  hex-expr
	  +bright
	  hsl-params
	  hsv-params))

(select-module color.model.rgb)

;; Utility Method & Functions
(define real->hex-string 
  (.$ (if$ (.$ (pa$ > 2) string-length)
	   (pa$ format "0~a")
	   identity)
      (cut number->string <> 16)
      x->integer
      floor
      (pa$ * 255)))

(define (m-values proc color)
  ((.$ (pa$ apply values)
       (map$ proc)
       x->list)
   color))

;;; Red Green Blue Model
(define-class* <rgb> (<color>)
  ([red   :is-a <real> :filter (inner$ 0 1)]
   [green :is-a <real> :filter (inner$ 0 1)]
   [blue  :is-a <real> :filter (inner$ 0 1)]))

(define-method +bright ((rgb <rgb>) (b <number>))
  (receive (r g b) (m-values (pa$ + b) rgb)
    (make <rgb> :red r :green g :blue b)))

(define-method hex-expr ((rgb <rgb>))
  ((.$ (fold$ (^ (x r) #`",r,x") "#")
       (map$ real->hex-string)
       x->list)
   rgb))

(define-method x->string ((rgb <rgb>))
  (receive (r g b) (m-values (.$ round->exact (pa$ * 100)) rgb)
    (format "rgb(~a%, ~a%, ~a%)" r g b)))

;;; RGBA (Added Alpha Channel)
(define-class* <rgba> (<rgb>)
  ([alpha :is-a <real> :filter (inner$ 0 1)]))

(define-method +bright ((rgba <rgba>) (b <real>))
  (let1 a (alpha-of rgba)
    (receive (r g b _) (m-values (pa$ + b) rgba)
      (make <rgba> :red r :green g :blue b :alpha a))))

(define-method x->string ((rgba <rgba>))
  (let1 a (alpha-of rgba)
    (receive (r g b _) (m-values (.$ round->exact (pa$ * 100)) rgba)
      (format "rgba(~a%, ~a%, ~a%, ~a)" r g b a))))

;; RGB -> RGBA
(define-method add-alpha ((rgb <rgb>) (a <real>))
  (receive (r g b) (x->values rgb)
    (make <rgba> :red r :green g :blue b :alpha a)))

;; RGBA -> RGB
(define-method remove-alpha ((rgba <rgba>))
  (receive (r g b _) (x->values rgba)
    (make <rgb> :red r :green g :blue b)))

;; Hue
(define (hue r g b . _)
  (let* ([max-val (max r g b)]
	 [min-val (min r g b)]
	 [c (- max-val min-val)])
    (* 60 (cond [(zero? c) 0]
		[(= max-val r) (fmod (/. (- g b) c) 6)]
		[(= max-val g) (+ (/. (- b r) c) 2)]
		[(= max-val b) (+ (/. (- r g) c) 4)]))))

;; 
(define (hsl-values r g b . _)
  (let* ([max-val (max r g b)]
	 [min-val (min r g b)]
	 [c (- max-val min-val)]
	 [l (/ (+ max-val min-val) 2)]
	 [s (if (zero? c) 0 (/ c (- 1 (abs (- (* 2 l) 1)))))]
	 [h (hue r g b)])
    (values h s l)))

;;
(define (hsv-values r g b . _)
  (let* ([max-val (max r g b)]
	 [min-val (min r g b)]
	 [c (- max-val min-val)]
	 [v max-val]
	 [s (if (zero? max-val) 0 (/. c v))]
	 [h (hue r g b)])
    (values h s v)))

;; Hue
(define-method hue-of ((rgb <rgb>))
  (apply hue (x->list rgb)))

;; HSL-Parameter
(define-method hsl-params ((rgb <rgb>))
  (apply hsl-values (x->list rgb)))

;; HSV-Parameter
(define-method hsv-params ((rgb <rgb>))
  (apply hsv-values (x->list rgb)))

(provide "color/model/rgb")
;; EOF