#!/usr/bin/env gosh

(define-module color.model.hsv
  (use util)
  (extend color.model.meta)
  (export <hsv> <hsva>))

(select-module color.model.hsv)

(define-class* <hsv> (<color>)
  ([hue        :type <number> :filter (loop-mod$ 360)]
   [saturation :type <number> :filter (inner$ 0 1)]
   [value      :type <number> :filter (inner$ 0 1)]))

(define-method getters ((hsv <hsv>))
  (list hue-of saturation-of value-of))

(define-class* <hsva> (<hsv>)
  ([alpha :type <number> :filter (inner$ 0 1)]))

(define-method getters ((hsva <hsva>))
  (list hue-of saturation-of value-of alpha-of))

;; Add & Remove Alpha Channel
(define-method add-alpha ((hsv <hsv>) (a <number>))
  (receive (h s v) (x->values hsv)
    (make <hsva> :hue h :saturation s :value v :alpha a)))

(define-method remove-alpha ((hsva <hsva>))
  (receive (h s v a) (x->values hsva)
    (make <hsv> :hue h :saturation s :value v)))

(provide "color/model/hsv")