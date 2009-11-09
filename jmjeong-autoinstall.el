;; auto-install
(require 'auto-install)
(setq auto-install-directory (concat elisp-root-dir "/auto-install/"))
;(auto-install-update-emacswiki-package-name t)
(setq auto-install-save-confirm nil)

(defalias 'aiw 'auto-install-from-emacswiki)
