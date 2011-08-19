#!/usr/bin/env gosh

(define-module color.model.meta
  (use util)
  (export-all))

(select-module color.model.meta)


(define-class <color> () (()) :metaclass <type-safe-meta>)

;; Method Interface
(define-method getters ((color <color>)) '())

(define-method x->list ((color <color>)) (map (cut <> color) (getters color)))

(define-method x->values ((color <color>)) (apply values (x->list color)))

(define-method add-alpha ((color <color>) (alpha <real>)) color)
(define-method remove-alpha ((color <color>)) color)

(provide "color/model/meta")
;; EOF