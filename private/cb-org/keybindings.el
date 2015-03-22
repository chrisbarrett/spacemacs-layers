(define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)
(define-key org-mode-map (kbd "M-p")     'org-metaup)
(define-key org-mode-map (kbd "M-n")     'org-metadown)
(define-key org-mode-map (kbd "C-c c")   'org-columns)
(define-key org-mode-map (kbd "C-c C-k") 'org/ctrl-c-ctrl-k)
(define-key org-mode-map (kbd "C-c RET") 'org/ctrl-c-ret)
(define-key org-mode-map (kbd "C-c ;")   nil)

(evil-define-key 'normal org-mode-map (kbd "RET") 'org-return)

;; Override clashing keybinding
(after 'flyspell
  (define-key flyspell-mode-map (kbd "C-c $") nil))

(bind-key* "<f12>" 'org-work-toggle-at-work)

(spacemacs/declare-prefix "o" "org")
(evil-leader/set-key "oa" 'org/agenda-dwim)
(evil-leader/set-key "oc" 'org-clock-goto)
(evil-leader/set-key "od" 'org/goto-diary)
(evil-leader/set-key "ok" 'org-capture)
(evil-leader/set-key "os" 'org-search-view)
(evil-leader/set-key "on" 'org/goto-notes)
(evil-leader/set-key "ow" 'org/goto-work)
(evil-leader/set-key "ot" 'org/todo-list)
(evil-leader/set-key "ov" 'org/tags-list)

(define-key org-agenda-mode-map (kbd "C-f") 'evil-scroll-page-down)
(define-key org-agenda-mode-map (kbd "C-b") 'evil-scroll-page-up)

(evil-leader/set-key "oh" 'helm-org-agenda-files-headings)
