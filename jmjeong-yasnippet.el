;; ;; yasnippet [2009-04-22]
(require 'yasnippet)

;; dont' map tab to yasnippet
;; set it to set to something we'll never use
(setq yas/trigger-key (kbd "C-`"))
(yas/initialize)

;; ;; Develop in ~/emacs.d/mysnippets, but also
;; ;; try out snippets in ~/Downloads/interesting-snippets
(setq yas/root-directory (list (concat default-directory "snippets")
						   (concat default-directory "yasnippet-0.5.10/snippets")))
;; ;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/root-directory)

defun



