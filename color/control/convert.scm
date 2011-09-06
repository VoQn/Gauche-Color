#!/usr/bin/env gosh
;; -*- mode: gauche; coding: utf-8; -*-

(define-module color.control.convert
  (use srfi-11)
  (use util)
  (use color.model)
  (export x->rgb
	  x->rgba
	  rgb->hsl
	  rgba->hsla
	  rgb->hsv
	  rgba->hsva))

(select-module color.control.convert)

;;; Color Model Converter

;; HCX -> RGB
(define-method hcx->rgb ((hcx <hcx>))
  (let*-values ([(h c x) (x->values hcx)]
		[(r g b) (case (x->integer (floor h))
			   [(0)  (values c x 0)]
			   [(1)  (values x c 0)]
			   [(2)  (values 0 c x)]
			   [(3)  (values 0 x c)]
			   [(4)  (values x 0 c)]
			   [(5)  (values c 0 x)]
			   [else (values 0 0 0)])])
    (make <rgb> :red r :green g :blue b)))

;; HSL -> HCX
(define-method hsl->hcx ((hsl <hsl>))
  (receive (_h s l) (x->values hsl)
    (let* ([h   (/. _h 60)]
	   [c   (* s (- 1 (abs (- (* l 2) 1))))]
	   [x   (* c (- 1 (abs (- (fmod h 2) 1))))])
      (make <hcx> :hue h :chroma c :x x))))

;; HSL -> RGB
(define-method hsl->rgb ((hsl <hsl>))
  (receive (_ s l) (x->values hsl)
    (let* ([c   (* s (- 1 (abs (- (* l 2) 1))))]
	   [m   (- l (/ c 2))])
      ((.$ (cut +bright <> m) hcx->rgb hsl->hcx) hsl))))


;; XA -> Y (convert without alpha-channel)
(define (convert-without-alpha obj conv)
  ((.$ conv remove-alpha) obj))

;; XA -> YA (convert with alpha-channel)
(define (convert/alpha obj conv)
  ((.$ (cut add-alpha <> (alpha-of obj)) conv remove-alpha) obj))


;; HSV -> HCX
(define-method hsv->hcx ((hsv <hsv>))
  (receive (_h s v) (x->values hsv)
    (let* ([h (/. _h 60)]
	   [c (* v s)]
	   [x (* c (- 1 (abs (- (fmod h 2) 1))))])
      (make <hcx> :hue h :chroma c :x x))))


;; HSV -> RGB
(define-method hsv->rgb ((hsv <hsv>))
  (receive (_ s v) (x->values hsv)
    (let* ([c (* v s)]
	   [m (- v c)])
      ((.$ (cut +bright <> m) hcx->rgb hsv->hcx) hsv))))


;; RGB -> HSL
(define-method rgb->hsl ((rgb <rgb>))
  (receive (h s l) (hsl-params rgb)
    (make <hsl> :hue h :saturation s :luminance l)))


;; RGBA -> HSL
(define-method rgba->hsla ((rgba <rgba>))
  (let1 a (alpha-of rgba)
    (receive (h s l) (hsl-params rgba)
      (make <hsla> :hue h :saturation s	:luminance l :alpha a))))

;; RGB -> HSV
(define-method rgb->hsv ((rgb <rgb>))
  (receive (h s v) (hsv-params rgb)
    (make <hsv> :hue h :saturation s :value v)))

(define-method rgba->hsva ((rgba <rgba>))
  (let1 a (alpha-of rgba)
    (receive (h s v) (hsv-params rgba)
      (make <hsva> :hue h :saturation s :value v :alpha a))))


(define-method x->rgb ((hcx <hcx>)) (hcx->rgb hcx))
(define-method x->rgb ((hsl <hsl>)) (hsl->rgb hsl))
(define-method x->rgb ((hsv <hsv>)) (hsv->rgb hsv))
(define-method x->rgb ((rgba <rgba>)) (remove-alpha rgba))
(define-method x->rgb ((hsla <hsla>)) (convert-without-alpha hsla hsl->rgb))
(define-method x->rgb ((hsva <hsva>)) (convert-without-alpha hsva hsv->rgb))
(define-method x->rgba ((hsla <hsla>)) (convert/alpha hsla x->rgb))
(define-method x->rgba ((hsva <hsva>)) (convert/alpha hsva x->rgb))

(provide "color/control/convert")
;; EOF