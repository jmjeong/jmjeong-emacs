;; (let ((type (emacs-type)))
;;   (cond ((eq type 'emacs-nt-window)
;; 	 ;; For Windows (emacs-23.x)
;; 	 (set-default-font "NanumGothicCoding-14")
;; 	 (setq-default line-spacing 2))
;; 	;; For Linux (emacs-23.x)
;; 	((eq type 'emacs-window)
;; 	 (set-default-font "Dina")
;;  	 (set-fontset-font "fontset-default" '(#x1100 . #xffdc) '("Gulimche" . "unicode-bmp")))
;; 	;; For Macintosh (Except Auqa, Carbon maybe 22.x)
;; 	;; [2009-03-19] mac에서 option키를 meta key로 remapping
;; 	((eq type 'emacs-mac-window)
;; 	 (set-default-font "Monaco-13")
;; 	 ;(setq-default ns-antialias-text t)
;; 	 (setq-default line-spacing 1))))

(let ((type (emacs-type)))
  ;; mac
  (cond ((eq type 'emacs-mac-window)
		 ;(set-default-font "NanumGothicCoding-14")
		 ; (set-face-font 'default "NanumGothicCoding-14")
		 ;; mac에서는 command 키를 meta key로 사용
		 (setq ns-command-modifier 'meta)
		 (setq ring-bell-function 'ignore)		 
		 ;; (setq ns-option-modifier 'command)
		 (add-to-list 'exec-path "/opt/local/bin")
		 (add-to-list 'exec-path "/usr/local/bin")
		 (add-to-list 'exec-path "/usr/texbin")
		 (setenv "PATH" (concat "/opt/local/bin" path-separator (getenv "PATH")))
		 (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
		 (setenv "PATH" (concat "/usr/texbin" path-separator (getenv "PATH"))))
		)
  ;; windows
  (cond ((eq type 'emacs-window)
		 ;; window에서는 벨 소리를 듣고 싶지 않다
		 (set-message-beep 'silent)
		 (set-face-font 'default "NanumGothicCoding-14")
		 ;; C-h p를 win32에서 프린트 하는 용도로 재지정
		 (global-set-key "\C-hp" 'w32-print-print-buffer-notepad))
		)
  )
