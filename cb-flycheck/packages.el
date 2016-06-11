;;; packages.el --- cb-flycheck Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'dash nil t)
  (require 'use-package nil t))

(defconst cb-flycheck-packages
  '(flycheck
    flycheck-pos-tip))

(defun cb-flycheck/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config
    (progn
      (setq flycheck-global-modes t)
      (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
      (setq flycheck-display-errors-delay 0.5)
      (setq flycheck-check-syntax-automatically '(mode-enabled idle-change save)))))

(defun cb-flycheck/post-init-flycheck-pos-tip ()
  (use-package flycheck-pos-tip
    :after flycheck
    :config
    (progn
      (setq flycheck-pos-tip-timeout 60)

      ;; Hide error popups if the error list buffer is showing.

      (defun cb-flycheck/error-list-showing? ()
        (-some->> (get-buffer flycheck-error-list-buffer)
                  (get-buffer-window-list)
                  (-keep #'window-frame)))

      (defun cb-flycheck/suppress-error-pos-tip (fn &rest args)
        (if (cb-flycheck/error-list-showing?)
            (apply flycheck-pos-tip-old-display-function args)
          (apply fn args)))

      (advice-add 'flycheck-pos-tip-error-messages :around #'cb-flycheck/suppress-error-pos-tip))))

;;; packages.el ends here
