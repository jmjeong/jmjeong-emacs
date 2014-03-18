;; markdown mode [2009-07-23]
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-hook 'markdown-mode-hook
		  (let ((original-command (lookup-key markdown-mode-map [tab])))
			`(lambda ()
			   (setq yas/fallback-behavior
					 '(apply ,original-command))
			   (auto-fill-mode)
			   (local-set-key [tab] 'yas/expand))))


(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("README$" . markdown-mode))

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (save-buffer)
  (async-shell-command
   (format "open -a /Applications/Marked.app %s"
       (shell-quote-argument (buffer-file-name))))
)

; markdown mode를 써보니 기존의 orgmode에 비해서 많이 불편하다.
; org-mode를 derive한 org-markdown-mode를 정의

;; (define-derived-mode org-markdown-mode org-mode "OrgMarkdown"
;;   "Major mode for OrgMarkdown.
;; \\{org-mode-map}"
;;   (setq case-fold-search nil)
;;   (set (make-local-variable 'before-save-hook) '((lambda() (convert-to-markdown-mode))))
;;   (set (make-local-variable 'after-save-hook) '((lambda() (convert-to-org-mode))))
;;   ;; (setq after-save-hook 'convert-to-org-mode)
;;   (setq org-indent-indentation-per-level 4)
;;   ;; (org-indent-mode t)
;;   (setq org-list-indent-offset 2)
;;   ;; (longlines-mode)
;;   )

;; (defun markdown-unset-tab ()
;;   "markdown-mode-hook"
;;   (define-key markdown-mode-map (kbd "<tab>") nil))
;; (add-hook 'markdown-mode-hook '(lambda() (markdown-unset-tab)))

;; (defun convert-to-markdown-mode ()
;;   "Convert org format into markdown format(fix header)"
;;   (interactive)
;;   (save-excursion
;; 	(progn
;; 	  (while (re-search-forward "^\\*\\*\\*\\*" nil t)        
;; 		(replace-match "####"))
;; 	  (goto-char (point-min))
;; 	  (while (re-search-forward "^\\*\\*\\*" nil t)        
;; 		(replace-match "###"))
;; 	  (goto-char (point-min))
;; 	  (while (re-search-forward "^\\*\\*" nil t)        
;; 		(replace-match "##"))
;; 	  (goto-char (point-min))	  
;; 	  (while (re-search-forward "^\\*" nil t)        
;; 		(replace-match "#"))  )))

;; (defun convert-to-org-mode ()
;;   "Convert markdown format into org format(fix header)"
;;   (interactive)
;;   (save-excursion
;; 	(progn
;; 	  (while (re-search-forward "^####" nil t)        
;; 		(replace-match "****"))
;; 	  (goto-char (point-min))	  
;; 	  (while (re-search-forward "^###" nil t)        
;; 		(replace-match "***"))
;; 	  (goto-char (point-min))	  
;; 	  (while (re-search-forward "^##" nil t)        
;; 		(replace-match "**"))
;; 	  (goto-char (point-min))	  
;; 	  (while (re-search-forward "^#" nil t)        
;; 		(replace-match "*"))  )))

;; (add-hook 'org-markdown-mode-hook '(lambda() (local-set-key "\C-cm" 'markdown-preview-file)))
;; (add-hook 'org-markdown-mode-hook '(lambda() (convert-to-org-mode)))