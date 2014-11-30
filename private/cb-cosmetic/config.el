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

(custom-set-variables
 '(lambda-symbol (string (make-char 'greek-iso8859-7 107))))

(add-hook 'cb:scheme-modes-hook    'lambda-mode)
(add-hook 'inferior-lisp-mode-hook 'lambda-mode)
(add-hook 'lisp-mode-hook          'lambda-mode)
(add-hook 'cb:elisp-modes-hook     'lambda-mode)
(add-hook 'cb:python-modes-hook    'lambda-mode)
