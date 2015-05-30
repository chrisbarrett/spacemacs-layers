;;; cb-org-latex-preview-retina.el --- Hack to export latex previews for retina displays  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

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

;; Hack org-latex-preview to also export previews at double size for retina
;; displays.

;;; Code:

(require 'org)
(require 'ox-latex)

(defconst cb-org-latex-preview-scaling-factor 2)

(defun cb-org-latex-preview-create-retina-image-with-dvipng (string tofile options buffer)
  (let* ((dest-file (format "%s@2X.%s" (file-name-sans-extension tofile) (file-name-extension tofile)))
         (base-path (make-temp-name (expand-file-name "orgtex" temporary-file-directory)))
         (tex-file (concat base-path ".tex"))
         (dvi-file (concat base-path ".dvi"))
         (png-file (concat base-path ".png"))
         (tex-content (cb-org-latex-preview--mk-latex-file-content string))

         (height (face-attribute 'default :height nil))
         (scale-prop (or (plist-get options (if buffer :scale :html-scale)) 1.0))
         (scale (* cb-org-latex-preview-scaling-factor scale-prop))
         (dpi (number-to-string (* scale (floor (* 0.9 (if buffer height 140.0))))))

         (fg (cb-org-latex-preview--fg-colour buffer options))
         (bg (cb-org-latex-preview--bg-colour buffer options)))
    (unwind-protect
        (condition-case err
            (cb-org-latex-preview--create
             tex-content dest-file tex-file dvi-file png-file dpi fg bg)
          (error
           (if org-format-latex-signal-error
               (error (error-message-string err))
             (message (error-message-string err)))))

      (cb-org-latex-preview--remove-intermediate-files base-path))))

(defun cb-org-latex-preview--mk-latex-file-content (str)
  (let ((header (org-create-formula--latex-header)))
    (format "%s\n\\begin{document}\n%s\n\\end{document}\n" header str)))

(defun cb-org-latex-preview--create (tex-content dest-file tex-file dvi-file png-file dpi fg bg)
  (with-temp-file tex-file (insert tex-content))

  (let ((dir default-directory))
    (ignore-errors
      (cd temporary-file-directory)
      (call-process "latex" nil nil nil tex-file))
    (cd dir))

  (cl-assert (file-exists-p dvi-file) nil "Failed to create dvi file from %s" tex-file)

  (ignore-errors
    (call-process "dvipng" nil nil nil
                  "-fg" fg "-bg" bg
                  "-D" dpi
                  "-T" "tight"
                  "-o" png-file
                  dvi-file))

  (cl-assert (file-exists-p png-file) nil "Failed to create png file from %s" png-file)

  (copy-file png-file dest-file 'replace)
  png-file)

(defun cb-org-latex-preview--fg-colour (buffer options)
  (let ((colour (or (plist-get options (if buffer :foreground :html-foreground))
                    "Black")))
    (cond
     ((equal colour 'default)
      (org-dvipng-color :foreground))
     ((equal colour "Transparent")
      colour)
     (t
      (org-dvipng-color-format colour)))))

(defun cb-org-latex-preview--bg-colour (buffer options)
  (let ((colour (or (plist-get options (if buffer :background :html-background))
                    "Transparent")))
    (cond
     ((equal colour 'default)
      (org-dvipng-color :background))
     ((equal colour "Transparent")
      colour)
     (t
      (org-dvipng-color-format colour)))))

(defun cb-org-latex-preview--remove-intermediate-files (base-path)
  (cl-loop for e in '(".dvi" ".tex" ".aux" ".log" ".png" ".out") do
           (when (file-exists-p (concat base-path e))
             (delete-file (concat base-path e)))))

(defadvice org-create-formula-image-with-dvipng (after export-retina-version activate)
  (apply 'cb-org-latex-preview-create-retina-image-with-dvipng
         (ad-get-args 0)))

(provide 'cb-org-latex-preview-retina)

;;; cb-org-latex-preview-retina.el ends here
