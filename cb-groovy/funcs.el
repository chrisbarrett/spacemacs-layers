;;; funcs.el --- Supporting functions for groovy layer  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun groovy/colon ()
  (interactive)
  (core/insert-smart-op-no-leading-space ":"))
