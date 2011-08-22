#!/usr/bin/env gosh

(define-module color.control.convert
  (use srfi-11)
  (use util)
  (extend color.model.hcx
	  color.model.rgb
	  color.model.hsl
	  color.model.hsv)
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
  (let*-values ([(_h c x) (x->values hcx)]
		[(r g b) (let1 h (x->integer (floor _h))
			   (case h
			       [(0)  (values c x 0)]
			       [(1)  (values x c 0)]
			       [(2)  (values 0 c x)]
			       [(3)  (values 0 x c)]
			       [(4)  (values x 0 c)]
			       [(5)  (values c 0 x)]
			       [else (values 0 0 0)]))])
    (make <rgb> :red r :green g :blue b)))

;; HSL -> HCX
(define-method hsl->hcx ((hsl <hsl>))
  (receive (_h s l) (x->values hsl)
    (let* ([h   (/. _h 60)]
	   [c   (* s (- 1 (abs (- (* l 2) 1))))]
	   [x   (* c (- 1 (abs (- (fmod h 2) 1))))])
      (make <hcx> :hue h :chroma c :x x))))

(define-method x->hcx ((hsl <hsl>)) (hsl->hcx hsl))

;; HSL -> RGB
(define-method hsl->rgb ((hsl <hsl>))
  (receive (_ s l) (x->values hsl)
    (let* ([c   (* s (- 1 (abs (- (* l 2) 1))))]
	   [m   (- l (/ c 2))])
      (+bright (hcx->rgb (hsl->hcx hsl))
	       m))))

;; XA -> Y (convert without alpha-channel)
(define (convert-without-alpha obj conv)
  (conv (remove-alpha obj)))

;; XA -> YA (convert with alpha-channel)
(define (convert/alpha obj conv)
  (add-alpha (conv (remove-alpha obj))
	     (alpha-of obj)))

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
      (+bright (hcx->rgb (hsv->hcx hsv)) m))))

;; RGB -> HSL
(define-method rgb->hsl ((rgb <rgb>))
  (receive (r g b) (x->values rgb)
    (let* ([max-val (max r g b)]
	   [min-val (min r g b)]
	   [c (- max-val min-val)]
	   [l (/ (+ max-val min-val) 2)]
	   [h (* 60 (cond [(zero? c) 0]
			  [(= max-val r) (fmod (/ (- g b) c) 6)]
			  [(= max-val g) (+ (/ (- b r) c) 2)]
			  [(= max-val b) (+ (/ (- r g) c) 4)]))]
	   [s (if (zero? c) 0 (/ c (- 1 (abs (- (* 2 l) 1)))))])
      (make <hsl> :hue h :saturation s :luminance l))))

;; RGBA -> HSL
(define-method rgba->hsla ((rgba <rgba>)) (convert/alpha rgba rgb->hsl))

;; RGB -> HSV
(define-method rgb->hsv ((rgb <rgb>))
  (receive (r g b) (x->values rgb)
    (let* ([max-val (max r g b)]
	   [min-val (min r g b)]
	   [c (- max-val min-val)]
	   [v max-val]
	   [s (if (zero? max-val) 0 (/. c v))]
	   [h (* 60 (cond [(zero? s) 0]
			  [(= max-val r) (/. (- g b) c)]
			  [(= max-val g) (/. (- b r) c)]
			  [(= max-val b) (/. (- r g) c)]))])
      (make <hsv> :hue h :saturation s :value v))))

(define-method rgba->hsva ((rgba <rgba>)) (convert/alpha rgba rgb->hsl))

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