(setq flycheck-scalastyle-jar "~/bin/scalastyle/lib/scalastyle_2.10-0.5.0.jar")
(setq flycheck-scalastylerc "~/bin/scalastyle/scalastyle_config.xml")
(setq flycheck-scala-scalastyle-executable "~/bin/scalastyle/scalastyle-batch_2.10.jar")

(add-to-list 'core/indent-commands-alist '(scala-mode . ensime-format-source))

(add-hook 'sbt-mode-hook 'turn-off-show-smartparens-mode)
(add-hook 'sbt-mode-hook (lambda () (show-paren-mode -1)))

(add-hook 'sbt-mode-hook 'cb-scala--apply-sbt-font-locking)

(defface cb-scala-sbt-pending-face
  `((t (:foreground ,solarized-hl-violet)))
  nil)

(defface cb-scala-sbt-error-face
  `((t (:foreground ,solarized-hl-orange)))
  nil)

(defface cb-scala-sbt-warning-face
  `((t (:foreground ,solarized-hl-yellow)))
  nil)

(defun cb-scala--apply-sbt-font-locking ()
  (font-lock-add-keywords
   nil
   `((,(rx bol (* space) (group "[info]" (* nonl)))
      1 'font-lock-comment-face t)

     (,sbt:prompt-regexp
      1 'font-lock-type-face t)

     (,(rx bol (* space) (group "[info] "))
      (0 '(face nil invisible t)))

     ;; Pending tests
     (,(rx bol (* space) "[info]" (+ space) (group "-" (+ space) (+ nonl)) (group "(pending)") eol)
      (1 'cb-scala-sbt-pending-face t)
      (2 '(face cb-scala-sbt-pending-face italic t) t))

     ;; Failing tests
     (,(rx bol (* space) "[info]" (+ space) (group "-" (+ nonl)) (group "*** FAILED ***"))
      (1 'cb-scala-sbt-error-face t)
      (2 '(face cb-scala-sbt-error-face italic t) t))

     ;; Ignored tests
     (,(rx bol (* space) "[info]" (+ space) (group "-" (+ space) (+ nonl)) (group "!!! IGNORED !!!") (* space) eol)
      (1 'cb-scala-sbt-warning-face t)
      (2 '(face cb-scala-sbt-warning-face italic t) t))

     ;; Stacktraces
     (,(rx bol (* space) (? "[error]" (* space)) (group "at " (+ nonl)))
      1 'font-lock-comment-face t)

     ;; Syntax Errors
     (,(rx bol (* space) "[error]" (* space) (group "^") (* space) eol)
      (1 '(face (:foreground ,solarized-hl-yellow) display "▲")))

     ;; Exceptions
     (,(rx bol (* space) (group (*? (any "." alpha)) ".") (group (+ alpha) "Exception") ":"
           (group (* nonl)))
      (1 '(face cb-scala-sbt-error-face display "⚫ "))
      (2 'font-lock-type-face t)
      (3 'font-lock-string-face t))


     ;; Re-compose notification levels
     (,(rx bol (* space) (group (or "[ERROR]" "[error]")))
      (1 '(face cb-scala-sbt-error-face display "⚫")))
     (,(rx bol (* space) (group (or "[WARN]" "[warn]")))
      (1 '(face cb-scala-sbt-warning-face display "⚫")))

     ;; Akka logging

     (,(rx bol (* space) "[ERROR]" (+ space)
           (group (and "[" (+ (not (any "]"))) "]" (+ space))) ; timestamp
           (group (+ (and "[" (+ (not (any "]"))) "]" (+ space)))) ; remaining fields
           (group (* nonl)))
      (1 '(face nil invisible t))
      (2 'font-lock-comment-face t)
      (3 'cb-scala-sbt-error-face t))

     (,(rx bol (* space) "[WARN]" (+ space)
           (group (and "[" (+ (not (any "]"))) "]" (+ space))) ; timestamp
           (group (+ (and "[" (+ (not (any "]"))) "]" (+ space)))) ; remaining fields
           (group (* nonl)))
      (1 '(face nil invisible t))
      (2 'font-lock-comment-face t)
      (3 'cb-scala-sbt-warning-face t))

     (,(rx (* space) "[INFO]" (+ space) (group (* nonl)))
      (0 'font-lock-comment-face t))

     (,(rx bol (* space)
           (? (+ (any digit ":" ".")) (+ space))
           (+ (and "[" (+ (not (any "]"))) "]" (+ space)))
           "DEBUG" (+ nonl) "\n")
      (0 '(face nil invisible t)))

     (,(rx bol (* space) (group (+ (any digit ":" "."))) (group (+ nonl)))
      (1 '(face nil invisible t))
      (2 'font-lock-comment-face t))

     ;; ES logging

     (,(rx bol (* space)
           (? (+ (any digit ":" ".")) (+ space))
           "[elasticsearch" (* nonl) " INFO " (* nonl) "\n")
      (0 '(face nil invisible t))))))
