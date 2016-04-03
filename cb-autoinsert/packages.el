;;; extensions.el --- cb-autoinsert Layer packages File for Spacemacs
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package nil t))

(defconst cb-autoinsert-packages
  '(autoinsert))

(defconst cb-autoinsert/elisp-gnu-license "
\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
")

(defun cb-autoinsert/init-autoinsert ()
  (use-package autoinsert
    :config
    (progn
      (setq auto-insert-alist
            '((("\\.ml\\'" . "OCaml Src File")
               nil
               "open Core.Std" "\n\n"
               _
               "\n")

              (("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
               (upcase
                (concat (f-filename (f-no-ext (buffer-file-name)))
                        "_"
                        (f-ext buffer-file-name)))
               "#ifndef " str n "#define " str "\n\n" _ "\n\n#endif")

              ((org-mode . "Org file")
               nil
               "#+TITLE: " (cb-autoinsert/org-title) "\n"
               "#+AUTHOR: " user-full-name           "\n"
               "\n")

              ((csharp-mode . "C# Src File")
               nil
               "using System;"                       "\n"
               "using System.Linq;"                  "\n"
               "using System.Collections.Generic;"   "\n"
               "\n"
               (cb-autoinsert/csharp-type-decl) " {" "\n"
               > _                                   "\n"
               "}"                                   "\n")

              ((emacs-lisp-mode . "Emacs Lisp")
               nil
               "\;;; " (f-filename (buffer-file-name)) " --- <enter description here>  "
               "-*- lexical-binding: t; -*-" '(setq lexical-binding t) \n
               \n
               ";; Copyright (C) " (format-time-string "%Y") "  "
               (getenv "ORGANIZATION") | user-full-name                \n
               \n
               ";; Author: " user-full-name " <" user-mail-address ">" \n
               cb-autoinsert/elisp-gnu-license                         \n
               ";;; Commentary:"                                       \n \n
               ";;; Code:"                                             \n \n
               _                                                       \n \n
               "\(provide '" (file-name-base) ")"                      \n \n
               "\;;; " (f-filename (buffer-file-name)) " ends here"    \n)

              ((html-mode . "HTML file")
               nil
               "<!DOCTYPE html>"          "\n"
               "<html>"                   "\n"
               "  <head>"                 "\n"
               "    <title>" _ "</title>" "\n"
               "  </head>"                "\n"
               "  <body>"                 "\n"
               "  </body>"                "\n"
               "</html>"                  "\n")))

      (setq auto-insert-query nil)
      (auto-insert-mode +1))))
