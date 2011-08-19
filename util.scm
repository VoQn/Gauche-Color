#!/usr/bin/env gosh

(define-module util
  (export-all))

(select-module util)

(define (tolerance t) (.$ (pa$ > t) abs -))

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
	     '())))


(define-class <type-safe-meta> (<class>) (()))

(define-method compute-get-n-set ((class <type-safe-meta>) slot)
  (let* ([has? (^ (option) (slot-definition-option slot option #f))]
	[acc (compute-slot-accessor class slot (next-method))]
	[type-error (^ (t v)
		       (error
			#`"Type Error : require type ,|t| but ,(class-of v)"
			v))]
	[validate-type (^v (if-let1 t (has? :is-a)
				    (if (is-a? v t) v (type-error t v))
				    v))]
	[_filter (^v (if-let1 f (has? :filter) ((.$ f validate-type) v) v))])
    (if (has? :is-a)
	(list (^ (o) (slot-ref-using-accessor o acc))
	      (^ (o v) (slot-set-using-accessor! o acc (_filter v)))
	      (^ (o) (slot-bound-using-accessor? o acc))
	      #t)
    (next-method))))

(define-syntax fif
  (syntax-rules ()
    [(_ pred true) (^ (x) (if (pred x) (true x)))]
    [(_ pred true false) (^ (x) (if (pred x) (true x) (false x)))]))

(define-syntax fpair
  (syntax-rules ()
    [(_ xs) (^ (f) (f (car xs) (cdr xs)))]
    [(_ xs proc) ((fpair xs) proc)]))

(define (loop-mod$ max) (^x (cond [(<= max x) (fmod x max)]
				  [(> 0 x) (fmod (+ x max) max)]
				  [else x])))

(define (inner$ min max) (cut clamp min <> max))

(define-method x->list ((obj <class>)) '())
(define-method x->list ((str <string>)) (string->list str))
(define-method x->list ((vec <vector>)) (vector->list vec))

(provide "util")
;; EOF
