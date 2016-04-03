;;; funcs.el --- Supporting funcs for auto-insert config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'f)

(defun cb-autoinsert/csharp-type-decl ()
  (let ((name (s-upper-camel-case (file-name-base))))
    (if (s-starts-with? "I" name)
        (format "interface %s" name)
      (format "class %s" name))))
