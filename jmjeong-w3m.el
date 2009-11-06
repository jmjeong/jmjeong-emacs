;; w3m mode [2009-04-14]
(autoload 'w3m                    "w3m" nil t)
(autoload 'w3m-browse-url         "w3m" nil t)
(if (= emacs-major-version 23)
	(require 'w3m-ems)
  (require 'w3m))

;; w3m width setting [2009-10-27]
(setq w3m-fill-column 120)

(setq browse-url-browser-function 'w3m-browse-url) ;; w3m
; (setq browse-url-default-browser "firefox.exe")    ;; firefox in M$ Windows

