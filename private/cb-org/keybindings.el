(define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)
(define-key org-mode-map (kbd "M-p")     'org-metaup)
(define-key org-mode-map (kbd "M-n")     'org-metadown)
(define-key org-mode-map (kbd "C-c c")   'org-columns)
(define-key org-mode-map (kbd "C-c C-k") 'cb-org/ctrl-c-ctrl-k)
(define-key org-mode-map (kbd "C-c RET") 'cb-org/ctrl-c-ret)
(define-key org-mode-map (kbd "C-c ;")   nil)
(define-key org-mode-map (kbd "M-C-g")   'org-plot/gnuplot)

(evil-define-key 'normal org-mode-map (kbd "RET") 'org-return)

;; Override clashing keybinding
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-c $") nil))

(bind-key* "<f12>" 'org-work-toggle-at-work)

(spacemacs/declare-prefix "o" "org")
(evil-leader/set-key "oa" 'cb-org/agenda-dwim)
(evil-leader/set-key "ob" 'org-iswitchb)
(evil-leader/set-key "oc" 'org-clock-goto)
(evil-leader/set-key "od" 'cb-org/goto-diary)
(evil-leader/set-key "ok" 'org-capture)
(evil-leader/set-key "os" 'org-search-view)
(evil-leader/set-key "on" 'cb-org/goto-notes)
(evil-leader/set-key "ow" 'cb-org/goto-work)
(evil-leader/set-key "ot" 'cb-org/todo-list)
(evil-leader/set-key "ov" 'cb-org/tags-list)

(define-key org-agenda-mode-map (kbd "C-f") 'evil-scroll-page-down)
(define-key org-agenda-mode-map (kbd "C-b") 'evil-scroll-page-up)

(evil-leader/set-key "oh" 'helm-org-agenda-files-headings)
(evil-leader/set-key-for-mode 'org-mode "mP" 'org-plot/gnuplot)
