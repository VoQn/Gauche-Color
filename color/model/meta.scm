#!/usr/bin/env gosh

(define-module color.model.meta
  (use srfi-1)
  (use util)
  (export-all))

(select-module color.model.meta)


(define-class <color> () () :metaclass <type-safe-meta>)


(define-method slots ((obj <color>))
  (let1 make-slot-name-list
      (^ (key)
	 (fold-right
	  (^ (x r) (if (null? (car x)) r (cons (car x) r)))
	  '()
	  (ref (class-of obj) key)))
    (let ((slots (make-slot-name-list 'slots))
	  (direct-slots (make-slot-name-list 'direct-slots)))
      (receive (intersection diff)
	  (lset-diff+intersection eq? slots direct-slots)
	(if (null? diff) intersection
	    (append intersection diff))))))

(define-method x->list ((obj <color>))
  (map (pa$ slot-ref obj) (slots obj)))

;; Method Interface
(define-method add-alpha ((color <color>) (alpha <real>)) color)
(define-method remove-alpha ((color <color>)) color)

(provide "color/model/meta")
;; EOF
