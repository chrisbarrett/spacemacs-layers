;;; funcs.el --- Supporting funcs for auto-insert config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'f)
(autoload 'projectile-project-p "projectile")

(defun cb-autoinsert/org-title ()
  "Format the title to use for the given FILENAME."
  (->> (file-name-base)
       s-split-words
       (-map 's-capitalized-words)
       (s-join " ")))

(defun cb-autoinsert/idris-module ()
  (s-capitalize (file-name-base)))

(defun cb-autoinsert/haskell-module ()
  (-if-let (root (and (buffer-file-name) (projectile-project-p)))

      (->> (f-no-ext (buffer-file-name))
           (s-chop-prefix root)
           (s-chop-prefix "app/")
           (s-chop-prefix "src/")
           (s-chop-prefix "test/")
           f-split
           (--map (let ((x (substring it 0 1))
                        (xs (substring it 1)))
                    (concat (s-upcase x) xs)))
           (s-join "."))

    (s-upper-camel-case (file-name-base))))

(defun cb-autoinsert/csharp-type-decl ()
  (let ((name (s-upper-camel-case (file-name-base))))
    (if (s-starts-with? "I" name)
        (format "interface %s" name)
      (format "class %s" name))))

(defun cb-autoinsert/scala-package ()
  (-if-let* ((root (and (buffer-file-name) (projectile-project-p)))
             (pkg-id (->> (f-dirname (buffer-file-name))
                          (s-chop-prefix root)
                          f-split
                          nreverse
                          (--take-while (not (-contains? '("src" "app" "scala" "test" "tests") it)))
                          nreverse
                          (s-join "."))))
      (if (s-blank? pkg-id)
          ""
        (format "package %s\n\n" pkg-id))
    ""))
