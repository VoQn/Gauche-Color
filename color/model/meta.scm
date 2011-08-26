#!/usr/bin/env gosh

(define-module color.model.meta
  (use srfi-1)
  (use util)
  (export-all))

(select-module color.model.meta)


(define-class <color> () () :metaclass <type-safe-meta>)

(define-method slots ((obj <color>))
  (let1 make-slot-name-list
      (^k (fold-right
		(^ (x r) (if (null? (car x)) r (cons (car x) r)))
		'()
		(ref (class-of obj) k)))
    (let ([slots (make-slot-name-list 'slots)]
	  [direct-slots (make-slot-name-list 'direct-slots)])
      (receive
	  (intersection diff)
	  (lset-diff+intersection eq? slots direct-slots)
	(if (null? diff) intersection
	    (append intersection diff))))))

(define-method x->list ((obj <color>))
  (map (pa$ slot-ref obj) (slots obj)))

;; Method Interface
(define-macro (define-method-interface name classes)
  `(define-method ,name ,classes (error #`"Not Support Method :" ,name)))


(define-method-interface hue-of ((color <color>)))
(define-method-interface hue-set! ((color <color>)))

(define-method-interface saturation-of ((color <color>)))
(define-method-interface saturation-set! ((color <color>)))

(define-method-interface alpha-of ((color <color>)))
(define-method-interface alpha-set! ((color <color>)))

(define-method-interface add-alpha ((color <color>) (alpha <real>)))
(define-method-interface remove-alpha ((color <color>)))

(provide "color/model/meta")
;; EOF
