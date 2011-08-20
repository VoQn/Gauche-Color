#!/usr/bin/env/ gosh

(use gauche.test)

(test-start "util")

(use util)
(test-module 'util)

(test-section "Utility Functions: tolerance")
(test* "case |x - y| < t" #t
       ((tolerance 1e-5) (/ 1 3) 0.33333333))
(test* "case |x - y| = t" #f
       ((tolerance 1e-5) 0 1e-5))
(test* "case |x - y| > t" #f
       ((tolerance 1e-5) 0 1e-4))

(test-section "Utility Functions: loop-mod$")
(test* "case x < 0"         10 ((loop-mod$ 360) -350) =)
(test* "case x = 0"          0 ((loop-mod$ 360) 0)    =)
(test* "case 0 < x < limit" 10 ((loop-mod$ 360) 10)   =)
(test* "case x = limit"      0 ((loop-mod$ 360) 360)  =)
(test* "case x > limit"     10 ((loop-mod$ 360) 370)  =)

(test-section "Utility Functions: inner$")
(test* "case x < min"       0 ((inner$ 0 10) -1) =)
(test* "case x = min"       0 ((inner$ 0 10)  0) =)
(test* "case min < x < max" 5 ((inner$ 0 10)  5) =)

(test-section "Utility Functions: x->list")
(test* "<string>" '(#\t #\e #\s #\t) (x->list "test"))
(test* "<vector>" '(1 2 3) (x->list '#(1 2 3)))

(test-section "Utility Macro: fif")
(test* "generate if-syntax : then case"
       "argument is odd! : 3"
       ((fif odd?
	     (^x #`"argument is odd! : ,x")
	     (^x #`"argument is even! : ,x"))
	3))

(test* "generate if-syntax : else case"
       "argument is even! : 4"
       ((fif odd?
	     (^x #`"argument is odd! : ,x")
	     (^x #`"argument is even! : ,x"))
	4))

(test* "generate if-syntax : without else"
       (undefined)
       ((fif odd?
	     (^x #`"argument is odd! : ,x"))
	4))


(test-section "Utility Macro: define-class*")
(test* "generating :init-value, :init-keyword, and accessor"
       '(define-class <test> ()
	  ((hoge :init-value 0
		 :init-keyword :hoge
		 :getter hoge-of
		 :setter hoge-set!
		 :is-a <number>))
	  :metaclass <type-safe-meta>)
       (macroexpand-1
	'(define-class* <test> ()
	   ((hoge :is-a <number>))
	   :metaclass <type-safe-meta>)))

(test* "generate pass when completed slot form"
       '(define-class <test> ()
	  ((hoge :is-a <number>
		 :init-value 1
		 :init-keyword :h
		 :getter get-hoge
		 :setter set!-hoge)))
       (macroexpand-1
	'(define-class* <test> ()
	   ((hoge :is-a <number>
		  :init-value 1
		  :init-keyword :h
		  :getter get-hoge
		  :setter set!-hoge)))))

(test* "case :read-only, without :setter accessor"
       `(define-class <test> ()
	  ((hoge :init-value ,(undefined)
		 :init-keyword :hoge
		 :getter hoge-of
		 :read-only #t)))
       (macroexpand-1
	'(define-class* <test> ()
	   ((hoge :read-only #t)))))

(test-section "Type Safe Meta Class")
(define-class <test-type-safe> ()
  ((hoge :is-a <number>
	 :init-value 0
	 :init-keyword :hoge
	 :filter (loop-mod$ 360)))
  :metaclass <type-safe-meta>)
(define test-obj (make <test-type-safe> :hoge 1))

(test* "is slot type safe" <number>
       (slot-ref test-obj 'hoge)
       (^ (c v) (is-a? v c)))

(test* "is slot auto filtering" 10
       (begin (set! (slot-ref test-obj 'hoge) -350)
	      (slot-ref test-obj 'hoge))
       =)

;; Test Error Handling
(set! *test-report-error* #f) 
(test* "is slot auto type validation"
       (test-error <error>)
       (set! (slot-ref test-obj 'hoge) "hoge"))


(test-end)