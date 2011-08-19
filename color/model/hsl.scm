#!/usr/bin/env gosh

(define-module color.model.hsl
  (use util)
  (extend color.model.meta)
  (export <hsl>
	  <hsla>))

(select-module color.model.hsl)

(define parcent-int (.$ round->exact (pa$ * 100)))

;;; HSL Color Model
(define-class* <hsl> (<color>)
  ([hue        :is-a <real> :filter (loop-mod$ 360)]
   [saturation :is-a <real> :filter (inner$ 0 1)]
   [luminance  :is-a <real> :filter (inner$ 0 1)]))

(define-method getters ((hsl <hsl>))
  (list hue-of saturation-of luminance-of))

(define-method x->string ((hsl <hsl>))
  (receive (h s l) (x->values hsl)
    (format "hsl(~a, ~a%, ~a%)"
	    (round->exact h)
	    (parcent-int s)
	    (parcent-int l))))

;;; HSLA Color Model
(define-class* <hsla> (<hsl>)
  ((alpha :is-a <real> :filter (inner$ 0 1))))

(define-method getters ((hsla <hsla>))
  (list hue-of saturation-of luminance-of alpha-of))

(define-method x->string ((hsla <hsla>))
  (receive (h s l a) (x->values hsla)
    (format "hsla(~a, ~a%, ~a%, ~a)"
	    (round->exact h)
	    (parcent-int s)
	    (parcent-int l)
	    a)))

;; HSL -> HSLA
(define-method add-alpha ((hsl <hsl>) (a <real>))
  (receive (h s l) (x->values hsl)
    (make <hsla> :hue h :saturation s :luminance l :alpha a)))

;; HSLA -> HSL
(define-method remove-alpha ((hsla <hsla>))
  (receive (h s l _) (x->values hsla)
    (make <hsl> :hue h :saturation s :luminance l)))

(provide "color/model/hsl")
;; EOF