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

(defun cb-org-latex-preview-create-retina-image-with-dvipng (string tofile options buffer)
  (let* ((tofile (format "%s@2X.%s" (file-name-sans-extension tofile) (file-name-extension tofile)))
         (texfilebase (make-temp-name (expand-file-name "orgtex" temporary-file-directory)))
         (texfile (concat texfilebase ".tex"))
         (dvifile (concat texfilebase ".dvi"))
         (pngfile (concat texfilebase ".png"))
         (fnh (face-attribute 'default :height nil))
         (scale (* 2 (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
         (dpi (number-to-string (* scale (floor (* 0.9 (if buffer fnh 140.))))))
         (fg (or (plist-get options (if buffer :foreground :html-foreground))
                 "Black"))
         (bg (or (plist-get options (if buffer :background :html-background))
                 "Transparent")))
    (if (eq fg 'default)
        (setq fg (org-dvipng-color :foreground))
      (unless (string= fg "Transparent") (setq fg (org-dvipng-color-format fg))))

    (if (eq bg 'default)
        (setq bg (org-dvipng-color :background))
      (unless (string= bg "Transparent") (setq bg (org-dvipng-color-format bg))))

    (let ((latex-header (org-create-formula--latex-header)))
      (with-temp-file texfile
        (insert latex-header)
        (insert "\n\\begin{document}\n" string "\n\\end{document}\n")))

    (let ((dir default-directory))
      (ignore-errors
        (cd temporary-file-directory)
        (call-process "latex" nil nil nil texfile))
      (cd dir))

    (if (not (file-exists-p dvifile))
        (progn (message "Failed to create dvi file from %s" texfile) nil)
      (ignore-errors
        (call-process "dvipng" nil nil nil
                      "-fg" fg "-bg" bg
                      "-D" dpi
                      ;;"-x" scale "-y" scale
                      "-T" "tight"
                      "-o" pngfile
                      dvifile))
      (if (not (file-exists-p pngfile))
          (if org-format-latex-signal-error
              (error "Failed to create png file from %s" texfile)
            (message "Failed to create png file from %s" texfile)
            nil)
        ;; Use the requested file name and clean up
        (copy-file pngfile tofile 'replace)
        (cl-loop for e in '(".dvi" ".tex" ".aux" ".log" ".png" ".out") do
                 (if (file-exists-p (concat texfilebase e))
                     (delete-file (concat texfilebase e))))
        pngfile))))

(defadvice org-create-formula-image-with-dvipng (after export-retina-version activate)
  (apply 'cb-org-latex-preview-create-retina-image-with-dvipng
         (ad-get-args 0)))

(provide 'cb-org-latex-preview-retina)

;;; cb-org-latex-preview-retina.el ends here
