
;; python mode
(load "python-mode" nil t)
(autoload 'python-mode "python-mode" "Python editing mode." t)

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                      interpreter-mode-alist))
(autoload 'python-mode "python" "Python editing mode." t)
(setq python-mode-hook
	  '(lambda () (progn
					(set-variable 'py-indent-offset 4)
					(set-variable 'py-smart-indentation nil)
					(set-variable 'indent-tabs-mode nil) ; python mode에서는 tab대신 space를
					(eldoc-mode 1))))

;; iPython settting
(setq ipython-command "/usr/local/bin/ipython")
(require 'ipython)
;; (setq py-python-command-args '("-colors" "NoColor")) 

;; ======================================================================
;; add pylookup to your loadpath
;; (add-to-list 'load-path "[PATH]")

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat elisp-root-dir "/vendor/pylookup/pylookup.py"))
(setq pylookup-db-file (concat elisp-root-dir "/vendor/pylookup/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup" 
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
