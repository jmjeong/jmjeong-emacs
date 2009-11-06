;; dired-x hooks [2009-10-09]
(add-hook 'dired-load-hook
		  (lambda ()
			(load "dired-x")
			;; Set dired-x global variables here.  For example:
			;; (setq dired-guess-shell-gnutar "gtar")
			;; (setq dired-x-hands-off-my-keys nil)
			))

