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
		   ,@(if (has? :init-value rest) '()
			 (list :init-keyword init-key))
		   :getter ,reader
		   ,@(if (has? :read-only rest) '()
			 (list :setter writer))
		   ,@rest)))
	   slots)
     ,@options))

(define (make-init-value key-list)
  (let1 has? (^k (get-keyword k key-list #f))
    (cond [(has? :type)
	   => (^t (case t
		    [(<number>) 0]
		    [(<string>) ""]
		    [(<boolean>) #f]
		    [else '()]))]
	  [else '()])))

(define-syntax fif
  (syntax-rules ()
    [(_ pred true) (^ (x) (if (pred x) (true x)))]
    [(_ pred true false) (^ (x) (if (pred x) (true x) (false x)))]))

(define-method minus? ((num <number>)) (> 0 num))
(define-method plus? ((num <number>)) (<= 0 num))

(define-syntax fpair
  (syntax-rules ()
    [(_ xs) (^ (f) (f (car xs) (cdr xs)))]
    [(_ xs proc) ((fpair xs) proc)]))

(define (loop-mod$ r) (^h (cond [(<= r h) (fmod h r)]
				[(minus? h) (fmod (+ h r) r)]
				[else h])))

(define (inner$ min max) (cut clamp min <> max))

(define-method x->list ((obj <class>)) '())

(provide "util")
;; EOF
