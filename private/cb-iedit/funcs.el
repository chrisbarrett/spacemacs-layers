(defun iedit/replace-read ()
  (save-excursion
    (iedit-goto-first-occurrence)
    (iedit-replace-occurrences (read-string "Replace in buffer: "))))

(defun iedit/number-of-occurrences ()
  "Return the number of active iedit occurrences."
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (until (eobp)
        (let ((pos (point))
              (in-occurrence (get-char-property (point) 'iedit-occurrence-overlay-name)))
          (when in-occurrence
            (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name)))
          (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name))
          (goto-char pos)
          (cl-incf n)))
      (1- n))))

(defun iedit/restrict-to-region ()
  (iedit-restrict-region (region-beginning) (region-end))
  (message "Restricted to %s matches in region."
           (iedit/number-of-occurrences)))

(defun iedit/remove-region ()
  (iedit-restrict-region (region-beginning) (region-end) t))

(defun iedit/restrict-to-window ()
  (let ((top (save-excursion
               (move-to-window-line 0)
               (point)))
        (bottom (save-excursion
                  (move-to-window-line -1)
                  (point))))
    (iedit-restrict-region top bottom)
    (message "Restricted to %s matches in visible area."
             (iedit/number-of-occurrences))))

(defun iedit/replace-in-region ()
  (iedit/restrict-to-region)
  (iedit/replace-read))
