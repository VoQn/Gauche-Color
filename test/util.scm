#!/usr/bin/env gosh

(define-module test.util
  (use srfi-1)
  (use gauche.test)
  (use util)
  (export-all))

(select-module test.util)

(define-syntax test-type*
  (syntax-rules ()
    [(_ name type expr) (test* name type (class-of expr))]))

(define-syntax test-list*
  (syntax-rules ()
    [(_ name exp expr)
     (test* name exp ((if$ pair? identity x->list) expr))]
    [(_ name exp expr test)
     (test* name exp ((if$ pair? identity x->list) expr)
	    (pa$ list= test))]))

;; testing each value between expect and result float list
(define enough? (tolerance$ 1e-5))

(provide "test/util")
;; EOF