#!/usr/bin/env gosh

(define-module color.model.rgb
  (use util)
  (use gauche.experimental.app)
  (extend color.model.meta)
  (export <rgb>
	  <rgba>
	  hex-expr
	  +bright))

(select-module color.model.rgb)

;; Utility Method & Functions
(define float->hex-string
  (.$ (fif (.$ (pa$ > 2) string-length)
	   (pa$ format "0~a")
	   identity)
      (cut number->string <> 16)
      x->integer
      floor
      (pa$ * 255)))

(define (m-values proc color)
  (apply values (map proc (x->list color))))

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
       (map$ float->hex-string)
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

(provide "color/model/rgb")
;; EOF