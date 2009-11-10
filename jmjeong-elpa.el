;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when (load (expand-file-name (concat elisp-root-dir
	"/vendor/elpa/package.el"))) (package-initialize))

(setq package-user-dir (concat elisp-root-dir "/vendor/elpa"))
