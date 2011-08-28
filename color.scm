#!/usr/bin/env gosh
;; -*- mode: gauche; coding: utf-8; -*-

(define-module color
  (extend color.model
	  color.control)
  (export-all))

(select-module color)

(provide "color")
;; EOF
