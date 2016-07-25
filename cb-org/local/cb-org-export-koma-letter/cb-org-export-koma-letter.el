;;; cb-org-export-koma-letter.el --- Export Koma letter with C-c C-c.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((f "0.17.2") (org "8.3.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds a C-c C-c handler for exporting the Koma letter at point.

;;; Code:

(require 'f)
(require 'org)
(require 'ox-latex)

(autoload 'cb-org-subtree-content "cb-org-subtree")
(autoload 'cb-org-subtree-write-content "cb-org-subtree")

(defconst cb-org-export-koma-letter-latex-class "
\\documentclass[paper=A4,pagesize,fromalign=right,
               fromrule=aftername,fromphone,fromemail,
               version=last]{scrlttr2}
\\usepackage[english]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[normalem]{ulem}
\\usepackage{booktabs}
\\usepackage{graphicx}
[NO-DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]")

;;;###autoload
(defun cb-org-export-koma-letter-at-subtree (dest)
  "Define a command to export the koma letter subtree at point to PDF.
With a prefix arg, prompt for the output destination. Otherwise
generate use the name of the current file to generate the
exported file's name. The PDF will be created at DEST."
  (interactive
   (list (if current-prefix-arg
             (ido-read-file-name "Destination: " nil nil nil ".pdf")
           (concat (f-no-ext (buffer-file-name)) ".pdf"))))

  (let ((tmpfile (make-temp-file "org-export-" nil ".org")))
    (cb-org-subtree-write-content tmpfile)
    (with-current-buffer (find-file-noselect tmpfile)
      (unwind-protect
          (-if-let (exported (org-koma-letter-export-to-pdf))
              (f-move exported dest)
            (error "Export failed"))
        (kill-buffer)))
    (async-shell-command (format "open %s" (shell-quote-argument dest)))
    (message "opening %s..." dest)))

(defun cb-org-export-koma-letter--handler ()
  "Export the koma letter at point."
  (when (ignore-errors
          (s-matches? (rx "latex_class:" (* space) "koma")
                      (cb-org-subtree-content)))
    (call-interactively 'cb-org-export-koma-letter-at-subtree)
    'export-koma-letter))

;;;###autoload
(defun cb-org-export-koma-letter-init ()
  (add-to-list 'org-latex-classes '("koma-letter" cb-org-export-koma-letter-latex-class))
  (add-hook 'org-ctrl-c-ctrl-c-hook #'cb-org-export-koma-letter--handler t))

(provide 'cb-org-export-koma-letter)

;;; cb-org-export-koma-letter.el ends here
