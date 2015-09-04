;;; extensions.el --- cb-haskell Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-haskell-pre-extensions
  '(
    haskell-parser
    )
  "List of all extensions to load before the packages.")

(defconst cb-haskell-post-extensions
  '(
    smart-ops
    liquid-types
    )
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'use-package nil t)
  (require 'smart-ops nil t))

(defun cb-haskell/init-smart-ops ()
  (use-package smart-ops
    :config
    (progn
      (defun cb-haskell/reformat-comment-at-point ()
        (-when-let ((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp)
                    (_ (equal op "{"))
                    (_ (s-matches? (rx "{" (* "-" space) "}")
                                   (buffer-substring beg end))))
          (goto-char beg)
          (delete-region beg end)
          (insert "{- ") (save-excursion (insert " -}"))))

      (defun cb-haskell/reformat-pragma-at-point ()
        (-when-let ((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp)
                    (_ (equal op "{"))
                    (_ (s-matches? (rx "{" (* "-" "#" space) "}")
                                   (buffer-substring beg end))))
          (goto-char beg)
          (delete-region beg end)
          (insert "{-# ") (save-excursion (insert " #-}"))))

      (defun cb-haskell/reformat-refinement-type-at-point ()
        (-when-let ((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp)
                    (_ (equal op "{"))
                    (_ (s-matches? (rx "{" (* "-" "@" space) "}")
                                   (buffer-substring beg end))))
          (goto-char beg)
          (delete-region beg end)
          (insert "{-@ ") (save-excursion (insert " @-}"))))

      (defun haskell/dot-accessing-module-or-constructor? ()
        (save-excursion
          (forward-char -1)
          (-when-let (sym (thing-at-point 'symbol))
            (s-uppercase? (substring sym 0 1)))))
      (defun cb-haskell/indent-if-in-exports ()
        (when (ignore-errors (s-matches? "ExportSpec" (elt (shm-current-node) 0)))
          (haskell-indentation-indent-line)))

      (defconst cb-haskell/smart-ops
        (-flatten-n 1
                    (list
                     (smart-ops "->" "=>")
                     (smart-ops "$" "=" "~" "^" ":" "..")
                     (smart-op "."
                               :pad-unless
                               (lambda (pt)
                                 (or
                                  (haskell/dot-accessing-module-or-constructor?)
                                  (equal (char-after) (string-to-char "}"))
                                  (funcall (smart-ops-after-match? (rx digit)) pt))))
                     (smart-op ";"
                               :pad-before nil :pad-after t)
                     (smart-ops ","
                                :pad-before nil :pad-after t
                                :action
                                #'cb-haskell/indent-if-in-exports)
                     (smart-op "-"
                               :action #'cb-haskell/reformat-comment-at-point)
                     (smart-op "#"
                               :pad-before nil :pad-after nil
                               :action #'cb-haskell/reformat-pragma-at-point)
                     (smart-op "@"
                               :pad-unless
                               (lambda (pt)
                                 (s-matches? "=" (buffer-substring (point) (line-end-position))))
                               :action 'cb-haskell/reformat-refinement-type-at-point)
                     (smart-ops-default-ops))))

      (define-smart-ops-for-mode 'haskell-mode
        cb-haskell/smart-ops)

      (define-smart-ops-for-mode 'haskell-interactive-mode
        (smart-op ":" :pad-unless (lambda (_) (haskell-interactive-at-prompt)))
        cb-haskell/smart-ops))))

(defun cb-haskell/init-haskell-parser ()
  (use-package haskell-parser
    :defer t
    :init
    (with-eval-after-load 'haskell-mode
      (require 'haskell-parser))))

(defun cb-haskell/init-liquid-types ()
  (use-package liquid-types
    :defer t
    :init
    (progn
      (defvar cb-haskell/use-liquid-haskell? nil)

      (defun cb-haskell/maybe-init-liquid-haskell ()
        (when (and cb-haskell/use-liquid-haskell? (executable-find "liquid"))
          (require 'flycheck-liquid)
          (require 'liquid-tip)
          (flycheck-add-next-checker 'haskell-ghc 'haskell-hlint)
          ;; (flycheck-add-next-checker 'haskell-hlint 'haskell-liquid)
          ;;(flycheck-select-checker 'haskell-liquid)
          ))

      (add-hook 'haskell-mode-hook #'cb-haskell/maybe-init-liquid-haskell)
      (add-hook 'literate-haskell-mode-hook #'cb-haskell/maybe-init-liquid-haskell))))
