;;; packages.el --- cb-cpp Layer extensions File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'evil nil t)
  (require 'thingatpt nil t)
  (require 'dash nil t)
  (require 'f nil t)
  (require 's nil t)
  (require 'use-package nil t))

(autoload 'indent-dwim-whole-buffer "indent-dwim")

(defconst cb-cpp-packages
  '(irony
    aggressive-indent
    company-irony
    company-irony-c-headers
    irony-eldoc
    flycheck-irony
    google-c-style
    ggtags
    helm-gtags
    cc-mode
    flyspell
    smart-ops
    (cpp-autoinsert :location local)))

(defun cb-cpp/init-irony ()
  (use-package irony
    :commands (irony-mode irony-install-server)
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    :config
    (progn
      (setq irony-user-dir (f-slash (f-join user-home-directory "bin" "irony")))
      (setq irony-server-install-prefix irony-user-dir)
      (setq irony-additional-clang-options '("-std=c++14"))

      (defun cb-cpp/irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

      (add-hook 'irony-mode-hook 'cb-cpp/irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))))

(defun cb-cpp/init-company-irony ()
  (use-package company-irony
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init
    (progn
      (push 'company-irony company-backends-c-mode-common)
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))))

(defun cb-cpp/init-irony-eldoc ()
  (use-package irony-eldoc
    :commands (irony-eldoc)
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(defun cb-cpp/init-flycheck-irony ()
  (use-package flycheck-irony
    :config
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))))

(defun cb-cpp/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init (push 'company-irony-c-headers company-backends-c-mode-common)))

(defun cb-cpp/init-google-c-style ()
  (use-package google-c-style
    :commands google-set-c-style
    :init
    (progn
      (setq-default clang-format-style "Google")
      (add-hook 'c-mode-common-hook 'google-set-c-style))))

(defun cb-cpp/post-init-ggtags ()
  (add-hook 'c++-mode-hook 'ggtags-mode)
  (with-eval-after-load 'ggtags
    (set-face-underline 'ggtags-highlight nil)))

(defun cb-cpp/post-init-helm-gtags ()
  (add-hook 'c++-mode-hook 'helm-gtags-mode))

(defun cb-cpp/post-init-aggressive-indent ()
  ;; Aggressive indent is a little too aggressive for C++.
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'c++-mode)))

(defun cb-cpp/post-init-cc-mode ()
  ;; HACK: The c-style way of setting up derived maps dynamically deeply
  ;; sucks. Just clobber that shit.
  (defconst c++-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "#"        'c-electric-pound)
      (define-key map "\C-c\C-e" 'c-macro-expand)
      (define-key map (kbd "M-RET") 'cb-cpp/M-RET)
      (define-key map (kbd "C-<return>") 'cb-cpp/C-RET)
      map))

  (evil-define-key 'insert c++-mode-map
    (kbd "<backspace>") 'sp-generic-prog-backspace
    (kbd "SPC") 'sp-generic-prog-space)

  (defun cb-cpp/set-local-hooks ()
    (add-hook 'before-save-hook 'indent-dwim-whole-buffer nil t))

  (add-hook 'c++-mode-hook 'cb-cpp/set-local-hooks)

  ;; Font-locking

  (font-lock-add-keywords
   'c++-mode
   `((";" 0 font-lock-comment-face t)
     ("\\_<constexpr\\_>" 0 font-lock-keyword-face t)
     ("\\_<noexcept\\_>" 0 font-lock-keyword-face t))))

(defun cb-cpp/post-init-flyspell ()
  (defun cb-cpp/flyspell-verify ()
    "Do not spellcheck imports."
    (and (flyspell-generic-progmode-verify)
         (not (s-matches? (rx bol (* space) "#") (cb-buffers-current-line)))))

  (defun cb-cpp/configure-flyspell ()
    (setq-local flyspell-generic-check-word-predicate 'cb-cpp/flyspell-verify))

  (add-hook 'c++-mode-hook 'cb-cpp/configure-flyspell))

(defun cb-cpp/post-init-smart-ops ()
  (defun cb-cpp/after-operator-keyword? (&rest _)
    (save-excursion
      (goto-char (smart-ops--maybe-beginning-of-op (smart-ops--rules-for-current-mode)))
      (thing-at-point-looking-at (rx bow "operator" eow (* space)))))

  (define-smart-ops-for-mode 'c++-mode
    (smart-ops
     "+" "-" "/" "%" "^" "|" "!" "=" "<<" ">>" "==" "!=" "&&" "||"
     "+=" "-=" "/=" "%=" "^=" "|=" "*=" "<<=" ">>=" "?"
     :pad-unless 'cb-cpp/after-operator-keyword?)

    ;; Pointers and templates
    (smart-ops "*>" "*>&" ">&" :pad-before nil :pad-after nil
               :action
               (lambda ()
                 (skip-chars-forward "*>&")))

    (smart-ops ":"
               :pad-unless
               (-orfn 'cb-cpp/after-operator-keyword?
                      (smart-ops-after-match? (rx (or "public" "private" "protected")))))

    (smart-ops "," :pad-before nil :pad-unless 'cb-cpp/after-operator-keyword?)
    (smart-ops "&" "*"
               :pad-before-if
               (smart-ops-after-match? (rx bow "return" eow (* space) eos))
               :pad-after-unless
               (smart-ops-after-match? (rx bow "return" eow (* space) (? (or "&" "*")) eos))
               :pad-unless 'cb-cpp/after-operator-keyword?)

    (smart-ops ";" :pad-before nil)
    (smart-ops "--" "++" :pad-before nil :pad-after nil)

    (smart-ops "." "::" "->" "->*" ">::"
               :pad-before nil :pad-after nil
               :action 'company-manual-begin)

    ;; Position point inside template braces.
    (smart-op "<>"
              :pad-before nil :pad-after nil
              :action (lambda (&rest _) (search-backward ">")))))

(defun cb-cpp/init-cpp-autoinsert ()
  (use-package cpp-autoinsert
    :functions (cpp-autoinsert-init)
    :config (cpp-autoinsert-init)))

;;; packages.el ends here
