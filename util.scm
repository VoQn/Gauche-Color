#!/usr/bin/env gosh
;; -*- mode: gauche; coding: utf-8; -*-

(define-module util
  (export-all))

(select-module util)


(define-macro (define-class* name supers slots . options)
  `(define-class ,name ,supers
     ,(map (^s (let ([has? (cut get-keyword <> <> #f)]
		     [key (car s)]
		     [accessor (^ (k a) (string->symbol #`",|k|-,|a|"))]
		     [rest (cdr s)])
		 (let1 compl (^ (k i)
			       (if (has? k rest) '()
				   (list k i)))
		   `(,key
		     ,@(compl :init-value (make-init-value rest))
		     ,@(compl :init-keyword (make-keyword key))
		     ,@(compl :getter (accessor key 'of))
		     ,@(if (or (has? :setter rest)
			       (has? :read-only rest)) '()
			       (list :setter (accessor key 'set!)))
		     ,@rest))))
	   slots)
     ,@options))

(define (make-init-value key-list)
  (let1 has? (cut get-keyword <> key-list #f)
    (if-let1 t
	     (has? :is-a)
	     (case t
	       [(<number> <complex> <real> <integer>) 0]
	       [(<string>) ""]
	       [(<boolean>) #f]
	       [(<list>) '()]
	       [(<vector>) '#()]
	       [else (make t)])
	     (undefined))))


(define-class <type-safe-meta> (<class>) (()))

(define-method compute-get-n-set ((class <type-safe-meta>) slot)
  (let* ([has? (cut slot-definition-option slot <> #f)]
	 [acc (compute-slot-accessor class slot (next-method))]
	 [type-error
	  (^ (value type)
	     (error
	      #`"Type Error : require type ,|type| but ,(class-of value)"
	      value))]
	 [validate-type
	  (^v (if-let1 t (has? :is-a)
		(if (is-a? v t) v (type-error v t))
		v))]
	 [validate-value
	  (^v (if-let1 validate (has? :validate) (validate v) v))]
	 [filter-value
	  (^v (if-let1 f (has? :filter) (f v) v))])
    (if (or (has? :is-a) (has? :validate) (has? :filter))
	(let1 filter/validate (.$ filter-value validate-value validate-type)
	  (list (^ (o) (slot-ref-using-accessor o acc))
		(^ (o v) (slot-set-using-accessor! o acc (filter/validate v)))
		(^ (o) (slot-bound-using-accessor? o acc))
		#t))
	(next-method))))

(define-syntax if$
  (syntax-rules ()
    [(_ pred true) (^ (x) (if (pred x) (true x)))]
    [(_ pred true false) (^ (x) (if (pred x) (true x) (false x)))]))

(define (tolerance$ t) (.$ (pa$ > t) abs -))

(define (loop-mod$ max) 
  (^x ((^v (if (= v (floor v)) (x->integer v) v))
       (cond [(<= max x) (fmod x max)]
	     [(> 0 x) (fmod (+ x max) max)]
	     [else x]))))

(define (inner$ min max) (cut clamp min <> max))

(define-method x->list ((str <string>)) (string->list str))
(define-method x->list ((vec <vector>)) (vector->list vec))

(define-method x->values ((obj <object>)) (apply values (x->list obj)))

(provide "util")
;; EOF
