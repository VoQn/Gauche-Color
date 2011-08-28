#!/usr/bin/env gosh
;; -*- mode: gauche; coding: utf-8; -*-

(define-module color.model
  (extend color.model.hcx
	  color.model.rgb
	  color.model.hsl
	  color.model.hsv)
  (export-all))

(select-module color.model)

(provide "color/model")