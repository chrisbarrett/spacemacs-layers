;;; extensions.el --- cb-idris Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(defconst cb-idris-pre-extensions
  '(smart-ops)
  "List of all extensions to load before the packages.")

(defconst cb-idris-post-extensions
  '()
  "List of all extensions to load after the packages.")

(eval-when-compile
  (require 'dash nil t)
  (require 'use-package nil t))

(defun cb-idris/init-smart-ops ()
  (use-package smart-ops
    :config
    (progn
      (defun cb-idris/looking-at-module-or-constructor? (&rest _)
        (-when-let ([fst] (thing-at-point 'symbol))
          (s-uppercase? fst)))

      (defun cb-idris/reformat-comment-at-point ()
        (-when-let* (((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
                     (_ (equal op "{"))
                     (_ (s-matches? (rx bos "{" (* (any "-" space)) "}" eos)
                                    (buffer-substring beg end))))
          (goto-char beg)
          (delete-region beg end)
          (insert "{- ") (save-excursion (insert " -}"))))

      (defconst cb-idris/smart-ops
        (-flatten-n 1
                    (list
                     (smart-ops "?" :pad-after nil)
                     (smart-ops "," :pad-before nil)
                     (smart-ops "$" "|" ":")
                     (smart-ops "." :pad-unless 'cb-idris/looking-at-module-or-constructor?)

                     (smart-ops "-"
                                :action #'cb-idris/reformat-comment-at-point)

                     ;; Reformat holes after `='.
                     (smart-ops "=?"
                                :pad-after nil
                                :action
                                (lambda (&rest _)
                                  (save-excursion
                                    (search-backward "?")
                                    (just-one-space))))

                     (smart-ops-default-ops))))

      (define-smart-ops-for-mode 'idris-mode cb-idris/smart-ops)
      (define-smart-ops-for-mode 'idris-repl-mode cb-idris/smart-ops))))
