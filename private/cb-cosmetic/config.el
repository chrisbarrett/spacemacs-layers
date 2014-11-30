;;; paren-face

(require 'paren-face)

(custom-set-faces
 '(parenthesis
   ((((background light)) :foreground "grey80")
    (((background dark))  :foreground "#505070"))))

(add-to-list 'paren-face-modes 'haskell-mode)
(add-to-list 'paren-face-modes 'inferior-haskell-mode)
(add-to-list 'paren-face-modes 'idris-mode)
(add-to-list 'paren-face-modes 'coq-mode)

(global-paren-face-mode)

;;; lambda-mode

(defvar lambda-symbol (string (make-char 'greek-iso8859-7 107)))

(diminish 'lambda-mode) 

(add-hook 'scheme-mode-hook        'lambda-mode)
(add-hook 'inferior-lisp-mode-hook 'lambda-mode)
(add-hook 'lisp-mode-hook          'lambda-mode)
(add-hook 'emacs-lisp-mode-hook    'lambda-mode)
(add-hook 'python-mode-hook        'lambda-mode)
