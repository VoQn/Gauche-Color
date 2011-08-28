#!/usr/bin/env gosh
;; -*- mode: gauche; coding: utf-8; -*-

(define-module color.control
  (extends color.control.convert
	   color.control.harmonics)
  (export-all))

(select-module color.control)

(provide "color/control")
;; EOF