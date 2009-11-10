;; Korean Setting
(when enable-multibyte-characters
;  (set-language-environment "Korean")
  
  ; (setq-default file-name-coding-system 'euc-kr)
  ;; the following setting is unnecessary from 20.5 >
  (when (string-match "^3" (or (getenv "HANGUL_KEYBOARD_TYPE") "390"))
    (setq default-korean-keyboard "3"))
   
  (setq input-method-verbose-flag nil
        input-method-highlight-flag nil)
  (prefer-coding-system 'utf-8)  		; utf-8 환경 설정
  ; (prefer-coding-system 'euc-kr)
  
  ; (set-buffer-file-coding-system 'euc-kr)
  ; (set-terminal-coding-system 'euc-kr)
  ; (set-default-coding-systems 'euc-kr)
  
  ; (setq default-process-coding-system '(euc-kr . euc-kr))
  (when window-system 
    (global-set-key "\C-\\" 'undefined))
  ;;  (add-hook 'quail-inactivate-hook 'delete-quail-completions)
  ;; (defun delete-quail-completions ()
  ;;   (when (get-buffer "*Quail Completions*")
  ;;     (kill-buffer "*Quail Completions*")))

  (unless window-system
    (set-keyboard-coding-system 'euc-kr)
    (set-terminal-coding-system 'euc-kr)
    (define-key encoded-kbd-mode-map [27] nil))

  ; (setq selection-coding-system 'euc-kr)
  ; (setq selection-coding-system 'ctext)

  ;; Hangul Mail setting
  ; (setq-default sendmail-coding-system 'euc-kr)

  ;; turn off C-h during input
  ;; (eval-after-load "quail"
  ;;   '(progn
  ;;      (define-key quail-translation-keymap "\C-h" 'quail-delete-last-char)
  ;;      (define-key quail-translation-keymap [tab] nil)
  ;;      (define-key quail-translation-keymap "\C-i" nil)	   
  ;;      (define-key quail-translation-keymap (kbd "C-SPC") 'set-mark-command)
  ;;      (define-key quail-translation-keymap "\C-?" 'quail-translation-help)))
  ;; (define-key global-map (kbd "C-x RET s") 'decode-coding-region))
  )
