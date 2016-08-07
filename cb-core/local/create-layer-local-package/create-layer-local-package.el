;;; create-layer-local-package.el --- Defines a command for creating a package local to a layer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (f "0.17.2") (dash "2.12.1") (smartparens "20160324.1541"))

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

;; Defines a command for creating a package local to a layer.

;;; Code:

(require 'dash)
(require 'eieio)
(require 'f)
(require 'rx)
(require 's)

(autoload 'helm "helm-descbinds")
(autoload 'sp-down-sexp "smartparens")
(autoload 'sp-end-of-previous-sexp "smartparens")
(autoload 'sp-end-of-sexp "smartparens")
(autoload 'sp-get-enclosing-sexp "smartparens")

(defconst create-layer-local-package-initialiser-format-string
  "
  (defun %s ()
    (use-package %s
      :config
      (progn)))
  ")

(defun create-layer-local-package--layers ()
  (--map (list :name (symbol-name it)
               :dir (configuration-layer/get-layer-path it))
         (with-no-warnings configuration-layer--used-layers)))

(defun create-layer-local-package--current-layer ()
  (-first (-lambda ((&plist :dir layer-dir))
            (or (f-same? default-directory layer-dir)
                (f-descendant-of? default-directory layer-dir)))
          (create-layer-local-package--layers)))

(defun create-layer-local-package--new-elisp-file-path (package-name layer)
  (format "%s%s" (f-join (plist-get layer :dir) "local" package-name package-name) ".el"))

(defun create-layer-local-package--create-elisp-file-at-path (elisp-file-path)
  (unless (f-exists? elisp-file-path)
    (make-directory (f-dirname elisp-file-path) t)
    (with-current-buffer (find-file elisp-file-path)
      (save-buffer))
    t))

(defun create-layer-local-package--raw-packages-list-at-pt ()
  (-let [(&plist :beg beg :end end) (sp-get-enclosing-sexp)]
    (read (buffer-substring beg end))))

(defun create-layer-local-package--package-names-in-packages-list (raw-packages-list)
  (--keep (cond ((consp it) (car it))
                ((symbolp it) it))
          raw-packages-list))

(defun create-layer-local-package--update-layer-package-list (package-name layer-name)
  (save-excursion
    (goto-char (point-min))
    (cond
     ((search-forward-regexp (rx-to-string `(and bol "(" (or "setq" "defconst") (+ space) ,(regexp-quote (concat layer-name "-packages")))))
      (sp-down-sexp)
      (let ((packages (create-layer-local-package--package-names-in-packages-list (create-layer-local-package--raw-packages-list-at-pt))))
        (unless (-contains? packages (intern package-name))
          (sp-end-of-sexp)
          (unless (null packages)
            (newline-and-indent))
          (insert (format "(%s :location local)" package-name))
          t)))
     (t
      (error "File %s does not contain a packages declaration" (buffer-name))))))

(defun create-layer-local-package--add-package-initialiser (package-name layer-name)
  (let ((initialiser-fn-name (format "%s/init-%s" layer-name package-name)))
    (goto-char (point-min))
    (unless (search-forward initialiser-fn-name nil t)

      ;; Move to end of final sexp in buffer.
      (goto-char (point-max))
      (sp-end-of-previous-sexp)
      (goto-char (line-end-position))

      (newline 2)
      (insert (format (s-trim create-layer-local-package-initialiser-format-string) initialiser-fn-name package-name))
      (newline)
      (forward-line 1)

      ;; Tidy up blank lines.
      (while (s-blank? (buffer-substring (line-beginning-position) (line-end-position)))
        (join-line t))

      (search-backward initialiser-fn-name)
      (goto-char (match-end 0))
      t)))

(defun create-layer-local-package--layer-packages-file (layer)
  (f-join (plist-get layer :dir) "packages.el"))

(defun create-layer-local-package--update-layer-packages-file (package-name layer)
  (-let [(&plist :name layer-name) layer]
    (with-current-buffer (find-file-noselect (create-layer-local-package--layer-packages-file layer))
      (list :layer-packages-updated-p
            (create-layer-local-package--update-layer-package-list package-name layer-name)
            :package-initialiser-added-p
            (create-layer-local-package--add-package-initialiser package-name layer-name)))))

(defun create-layer-local-package--make-local-package (package-name layer)
  (-concat
   (list :elisp-file-created-p
         (create-layer-local-package--create-elisp-file-at-path (create-layer-local-package--new-elisp-file-path package-name layer)))
   (create-layer-local-package--update-layer-packages-file package-name layer)))

(defun create-layer-local-package--display-package-file-and-initialiser (package-name layer)
  (delete-other-windows)
  (find-file (create-layer-local-package--new-elisp-file-path package-name layer))
  (display-buffer (find-file-noselect (create-layer-local-package--layer-packages-file layer))))

(defun create-layer-local-package--read-package-name (&optional initial-input)
  (let ((input (read-string "Package name: " initial-input)))
    (if (s-matches? (rx bos (+ (any alnum "-")) eos) input)
        input
      (message "Invalid package name. May contain only hyphens and alphanumeric chars")
      (sit-for 1.2)
      (create-layer-local-package--read-package-name input))))

(defun create-layer-local-package--layer-with-name (layers layer-name)
  (--first (equal layer-name (plist-get it :name)) layers))

(defun create-layer-local-package--report-results (results)
  (let* ((steps
          (-map (-lambda ((step performed))
                  (list (pcase step
                          (`:package-initialiser-added-p "Added package initialiser")
                          (`:elisp-file-created-p "Created Elisp file")
                          (`:layer-packages-updated-p "Updated layer package list"))
                        (if performed "done" "skipped")))
                (-partition 2 results)))

         (longest-step-width
          (-max (-map (-lambda ((step _)) (length step)) steps)))

         (padded (-map (-lambda ((step p))
                         (concat (s-pad-right (+ 6 longest-step-width) " " step) p))
                       steps)))
    (message "%s" (s-join "\n" padded))))

;;;###autoload
(defun create-layer-local-package ()
  "Interactively create a new local package."
  (interactive)
  (let* ((layers (create-layer-local-package--layers))
         (source `((name . "Layers")
                   (candidates . ,(--map (plist-get it :name) layers))
                   (action . ,(-lambda (layer-name)
                                (let* ((package-name (create-layer-local-package--read-package-name))
                                       (layer (create-layer-local-package--layer-with-name layers layer-name))
                                       (results (create-layer-local-package--make-local-package package-name layer)))
                                  (create-layer-local-package--display-package-file-and-initialiser package-name layer)
                                  (create-layer-local-package--report-results results)))))))
    (helm :sources (list source)
          :preselect (plist-get (create-layer-local-package--current-layer) :name)
          :buffer "*create local package*"
          :prompt "Create in layer: ")))

(provide 'create-layer-local-package)

;;; create-layer-local-package.el ends here
