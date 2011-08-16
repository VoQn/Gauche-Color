#!/usr/bin/env gosh

(define-module color.control.convert
  (use util)
  (use gauche.experimental.app)
  (extend color.model.hcx
	  color.model.rgb
	  color.model.hsl
	  color.model.hsv)
  (export x->rgb
	  x->rgba
	  rgb->hsl
	  rgba->hsla))

(select-module color.control.convert)

;;; Color Model Converter

;; HCX -> RGB
(define-method hcx->rgb ((hcx <hcx>))
  (receive (_h c x) (x->values hcx)
    (receive (r g b)
	(let1 h ($ x->integer $ floor _h)
	  ($* values $ case h
		       [(0)  (list c x 0)]
		       [(1)  (list x c 0)]
		       [(2)  (list 0 c x)]
		       [(3)  (list 0 x c)]
		       [(4)  (list x 0 c)]
		       [(5)  (list c 0 x)]
		       [else (list 0 0 0)]))
      (make <rgb> :red r :green g :blue b))))


;; HSL -> RGB
(define-method hsl->rgb ((hsl <hsl>))
  (receive (_h s l) (x->values hsl)
    (let* ([h   (/. _h 60)]
	   [c   (* s (- 1 (abs (- (* l 2) 1))))]
	   [x   (* c (- 1 (abs (- (fmod h 2) 1))))]
	   [m   (- l (/ c 2))]
	   [rgb (hcx->rgb (make <hcx> :hue h :chroma c :x x))])
      (+bright rgb m))))

;; HSLA -> RGBA
(define-method hsla->rgba ((hsla <hsla>))
  (receive (_ _ _ a) (x->values hsla)
    (let1 hsl (remove-alpha hsla)
      (add-alpha (hsl->rgb hsl) a))))


;; HSV -> RGB
(define-method hsv->rgb ((hsv <hsv>))
  (receive (_h s v) (x->values hsv)
    (receive (r g b)
	(apply values
	       (if (zero? s) (list v v v)
		   (let* ([h (x->integer (floor (/. _h 60)))]
			  [f (- (/. _h 60) h)]
			  [m (* v (- 1 s))]
			  [n (* v (- 1 (* s f)))]
			  [k (* v (- 1 (* s (- 1 f))))])
		     (case h
		       [(0) (list v k m)]
		       [(1) (list n v m)]
		       [(2) (list m v k)]
		       [(3) (list m n v)]
		       [(4) (list k m v)]
		       [(5) (list v m n)]))))
      (make <rgb> :red r :green g :blue b))))

;; HSVA -> RGBA
(define-method hsva->rgba ((hsva <hsva>))
  (receive (_ _ _ a) (x->values hsva)
    (let1 hsv (remove-alpha hsva)
      (add-alpha (hsv->rgb hsv) a))))

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
(define-method rgba->hsla ((rgba <rgba>))
  (receive (_ _ _ a) (x->values rgba)
    (let1 rgb (remove-alpha rgba)
      (add-alpha (rgb->hsl rgb) a))))

(define-method x->rgb ((hcx <hcx>)) (hcx->rgb hcx))
(define-method x->rgb ((hsl <hsl>)) (hsl->rgb hsl))
(define-method x->rgb ((hsv <hsv>)) (hsv->rgb hsv))
(define-method x->rgba ((hsla <hsla>)) (hsla->rgba hsla))
(define-method x->rgba ((hsva <hsva>)) (hsva->rgba hsva))

(provide "color/control/convert")
;; EOF