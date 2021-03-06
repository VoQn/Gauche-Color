#!/usr/bin/env gosh

(define-module color.model.hcx
  (use util)
  (extend color.model.meta)
  (export <hcx>))

(select-module color.model.hcx)
;;; Hue Chroma X Model
 
(define-class* <hcx> (<color>)
  ((hue    :is-a <real> :filter (loop-mod$ 6))
   (chroma :is-a <real> :filter (inner$ 0 1))
   (x      :is-a <real> :filter (inner$ 0 1))))

(provide "color/model/hcx")