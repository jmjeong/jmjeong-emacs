;; anything
(require 'anything)
(require 'anything-config)

(setq anything-sources
      '(anything-c-source-buffers
        anything-c-source-buffer-not-found
        anything-c-source-file-name-history
		anything-c-source-file-cache
        ;; anything-c-source-info-pages
        ;; anything-c-source-info-elisp
        ; anything-c-source-man-pages
        ; anything-c-source-locate
        ; anything-c-source-emacs-commands
		; anything-c-source-bbdb
		; anything-c-source-mac-spotlight
        ))

(global-set-key (kbd "M-+") 'anything)