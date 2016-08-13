;;; cb-org-gdrive.el --- Google drive exporter for orgmode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; Package-Requires: ((s "1.10.0") (f "0.17.2") (dash "2.12.1") (org "8.3.4"))

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

;; Provides commands for uploading and importing documents into Google Drive,
;; along with an org exporter. Requires the `gdrive' program.
;;
;;   https://github.com/prasmussen/gdrive
;;
;; The org ODT exporter is used to create an intermediate document, which is
;; uploaded to Drive and converted to a Google Doc.

;;; Installation:

;; Install this package and add the following to your Emacs config:
;;
;;   (add-hook 'org-mode-hook #'cb-org-gdrive-init)
;;
;; Below is an appropriate `use-package' form:
;;
;;   (use-package cb-org-gdrive
;;     :after org
;;     :config
;;     (add-hook 'org-mode-hook #'cb-org-gdrive-init))

;;; Code:

(require 'dash)
(require 'f)
(require 'ox)
(require 's)
(require 'ox-odt)

(defgroup cb-org-gdrive nil
  "Google Drive importer for orgmode's export dispatcher."
  :group 'org
  :prefix "cb-org-gdrive-")

(defcustom cb-org-gdrive-program "gdrive"
  "The path to the gdrive executable."
  :group 'cb-org-gdrive
  :type 'string)

(defcustom cb-org-gdrive-leave-source-after-import nil
  "Whether leave the intermediate ODT on the filesystem after importing."
  :group 'cb-org-gdrive
  :type 'boolean)

(defconst cb-org-gdrive--proc-name "gdrive")

(defconst cb-org-gdrive--proc-buffer "*gdrive*")

(defconst cb-org-gdrive--document-url-base "https://docs.google.com/document/d/")

(defun cb-org-gdrive--mk-upload-sentinel (file delete-original? cont)
  (lambda (proc _)
    (let ((proc (get-process proc)))
      (pcase (process-exit-status proc)
        (0
         (condition-case-unless-debug _err
             (progn
               (when delete-original?
                 (ignore-errors (kill-buffer file))
                 (f-delete file t))
               (message "Upload to drive completed successfully."))
           (error
            (error "File uploaded, but deleting intermediate file failed")))
         (funcall cont file))

        (status
         (let ((note (with-current-buffer (process-buffer proc)
                       (goto-char (point-max))
                       (buffer-substring (line-beginning-position) (line-end-position)))))
           (message "Upload to drive failed. status: %s, reason: %s" status note)))))))

;;;###autoload
(defun cb-org-gdrive-upload (file &optional delete-original?)
  "Upload FILE to google drive.

If DELETE-ORIGINAL? is set, delete the source file after a
successful upload."
  (interactive "fFile: ")
  (let ((proc (start-process cb-org-gdrive--proc-name cb-org-gdrive--proc-buffer cb-org-gdrive-program "upload" "--recursive" file)))
    (message "Uploading to Drive...")
    (set-process-sentinel proc (cb-org-gdrive--mk-upload-sentinel file delete-original? #'ignore))))

;;;###autoload
(defun cb-org-gdrive-import (file cont &optional delete-original?)
  "Import FILE into Google Drive, converting it to a Google Doc.

If DELETE-ORIGINAL? is set, delete the source file after a
successful upload.

CONT is executed on success and is passed the path of the uploaded file."
  (interactive "fFile: ")
  (let ((proc (start-process cb-org-gdrive--proc-name cb-org-gdrive--proc-buffer cb-org-gdrive-program "import" file)))
    (message "Uploading to Drive...")
    (set-process-sentinel proc (cb-org-gdrive--mk-upload-sentinel file delete-original? cont))))

(defun cb-org-gdrive--export-to-odt-and-import (&optional _async subtreep visible-only ext-plist)
  "Export the current buffer and upload it to Google Drive.

The file will be converted to Google Docs' internal format.
The intermediate file will be deleted unless
`cb-org-gdrive-leave-source-after-import' is non-nil.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

The ASYNC argument is not supported by this exporter and is
ignored.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return a plist with the following keys:

  :proc    -- The process performing the upload to Drive
  :file    -- The path to the intermediate ODT file
  :delete? -- Whether the intermediate file will be deleted after upload."
  (interactive)
  (run-exporter #'ignore subtreep visible-only ext-plist))

(defun cb-org-gdrive--run-exporter (callback &optional subtreep visible-only ext-plist)
  (-if-let (file (save-excursion (org-odt-export-to-odt nil subtreep visible-only)))
      (let ((delete? (not cb-org-gdrive-leave-source-after-import)))
        (list :proc (cb-org-gdrive-import file callback delete?)
              :file file
              :delete? delete?))
    (error "Exporting to ODT failed")))

(defun cb-org-gdrive--find-file-url (file)
  (-when-let* ((query (format "trashed = false and name = '%s'" (file-name-nondirectory file)))
               ((lines &as header match) (process-lines cb-org-gdrive-program "list" "--query" query))
               ((id) (s-split (rx space) match t)))
    (concat cb-org-gdrive--document-url-base id)))

(defun cb-org-gdrive--export-to-odt-and-open (&optional _async subtreep visible-only ext-plist)
  "See `cb-org-gdrive--export-to-odt-and-import'."
  (interactive)
  (let ((cont (lambda (file)
                (-if-let (url (cb-org-gdrive--find-file-url file))
                    (browse-url url)
                  (error "Could not determine Drive URL for exported file")))))
    (cb-org-gdrive--run-exporter cont subtreep visible-only ext-plist)))

;;;###autoload
(defun cb-org-gdrive-init ()
  (org-export-define-derived-backend 'gdrive 'odt
    :export-block '("GDRIVE" "GOOGLE DRIVE")
    :menu-entry
    '(?d "Import to Google Drive"
         ((?d "Import" cb-org-gdrive--export-to-odt-and-import)
          (?o "Import and open" cb-org-gdrive--export-to-odt-and-open))))
  (add-to-list 'org-export-backends 'gdrive))

(provide 'cb-org-gdrive)

;;; cb-org-gdrive.el ends here
