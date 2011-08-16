#!/usr/bin/env gosh

(define-module color.model.hcx
  (use util)
  (extend color.model.meta)
  (export <hcx>))

(select-module color.model.hcx)
;;; Hue Chroma X Model
 
(define-class* <hcx> (<color>)
  ((hue    :type <number> :filter (loop-mod$ 360))
   (chroma :type <number> :filter (inner$ 0 1))
   (x      :type <number> :filter (inner$ 0 1))))

(define-method getters ((hcx <hcx>)) (list hue-of chroma-of x-of))

(provide "color/model/hcx")