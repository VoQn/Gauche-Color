#!/usr/bin/env gosh

(define-module util
  (export-all))

(select-module util)


(define-macro (define-class* name supers slots . options)
  `(define-class ,name ,supers
     ,(map (^s (let* ([has? (^ (key list) (get-keyword key list #f))]
		      [key (car s)]
		      [init-key (make-keyword key)]
		      [accessor (^a (string->symbol #`",|key|-,|a|"))]
		      [reader (accessor 'of)]
		      [writer (accessor 'set!)]
		      [rest (cdr s)])
		 `(,key
		   ,@(if (has? :init-value rest) '()
			 (list :init-value (make-init-value rest)))
		   ,@(if (has? :init-keyword rest) '()
			 (list :init-keyword init-key))
		   ,@(if (has? :getter rest) '()
			 (list :getter reader))
		   ,@(if (or (has? :setter rest)
			     (has? :read-only rest)) '()
			 (list :setter writer))
		   ,@rest)))
	   slots)
     ,@options))

(define (make-init-value key-list)
  (let1 has? (^k (get-keyword k key-list #f))
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
  (let* ([has? (^ (option) (slot-definition-option slot option #f))]
	 [acc (compute-slot-accessor class slot (next-method))]
	 [type-error
	  (^ (type value)
	     (error
	      #`"Type Error : require type ,|type| but ,(class-of value)"
	      value))]
	 [validate-type
	  (^v (if-let1 t (has? :is-a)
		       (if (is-a? v t) v (type-error t v))
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

(define-syntax fif
  (syntax-rules ()
    [(_ pred true) (^ (x) (if (pred x) (true x)))]
    [(_ pred true false) (^ (x) (if (pred x) (true x) (false x)))]))

(define (tolerance t) (.$ (pa$ > t) abs -))

(define (loop-mod$ max) (^x (cond [(<= max x) (fmod x max)]
				  [(> 0 x) (fmod (+ x max) max)]
				  [else x])))

(define (inner$ min max) (cut clamp min <> max))

(define-method x->list ((str <string>)) (string->list str))
(define-method x->list ((vec <vector>)) (vector->list vec))

(define-method x->values ((obj <object>)) (apply values (x->list obj)))

(provide "util")
;; EOF
