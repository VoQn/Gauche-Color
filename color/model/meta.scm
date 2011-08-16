#!/usr/bin/env gosh

(define-module color.model.meta
  (use util)
  (use gauche.collection)
  (use gauche.mop.validator)
  (export <color>
	  getters
	  x->list
	  x->values
	  add-alpha
	  remove-alpha))

(select-module color.model.meta)

(define-class <filter-meta> (<class>) (()))

(define-method compute-get-n-set ((class <filter-meta>) slot)
  (let ([has? (^ (option) (slot-definition-option slot option #f))]
	[acc (compute-slot-accessor class slot (next-method))])
    (cond [(has? :filter)
	   => (^f (list (^ (o) (slot-ref-using-accessor o acc))
			(^ (o v) (slot-set-using-accessor! o acc (f v)))
			(^ (o) (slot-bound-using-accessor? o acc))
			#t))]
	  [else (next-method)])))

(define-class <color> ()
  (())
  :metaclass <filter-meta>)


(define-method getters ((color <color>)) '())

(define-method x->list ((color <color>))
  (map (cut <> color) (getters color)))

(define-method x->values ((color <color>))
  (apply values (x->list color)))

(define-method add-alpha ((color <color>)) color)
(define-method remove-alpha ((color <color>)) color)

(provide "color/model/meta")
;; EOF