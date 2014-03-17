;; python mode
;(autoload 'python-mode "python-mode" "Python editing mode." t)

;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                       interpreter-mode-alist))

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(autoload 'python-mode "python" "Python editing mode." t)
(setq python-mode-hook
	  '(lambda () (progn
					(set-variable 'py-indent-offset 4)
					(set-variable 'py-smart-indentation nil)
					(set-variable 'indent-tabs-mode nil) ; python mode에서는 tab대신 space를
					(set-variable 'py-load-pymacs-p nil)
					(eldoc-mode 1))))

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; iPython settting
;; (setq ipython-command "/usr/local/bin/ipython")
;; (require 'ipython)
;; (setq py-python-command-args '("-colors" "NoColor")) 

;; ======================================================================
;; add pylookup to your loadpath
;; (add-to-list 'load-path "[PATH]")

;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))

;; ;; set executable file and db file
;; (setq pylookup-program (concat elisp-root-dir "/vendor/pylookup/pylookup.py"))
;; (setq pylookup-db-file (concat elisp-root-dir "/vendor/pylookup/pylookup.db"))

;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)

;; (autoload 'pylookup-update "pylookup" 
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)
