;; c-mode
;;
(add-hook 'c-mode-common-hook 'jmjeong-c-mode-init)
(add-hook 'c++-mode-common-hook 'jmjeong-c-mode-init)
(defun jmjeong-c-mode-init ()
  (c-toggle-auto-hungry-state 1)
  (c-toggle-auto-newline 0)
  (c-set-style "stroustrup")
  (modify-syntax-entry ?_ "w")       ; now '_' is not considered a word-delimiter
  (c-set-offset 'member-init-intro '++)
  (setq c-tab-always-indent nil)
  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                 "\\|protected slot\\|private\\|private slot"
                                 "\\)\\>")
        c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                 "\\|public slots\\|protected slots\\|private slots"
                                 "\\)\\>[ \t]*:"))
  (progn
    ;; modify the colour of slots to match public, private, etc ...
    (font-lock-add-keywords 'c++-mode
                            '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
    ;; make new font for rest of qt keywords
    (make-face 'qt-keywords-face)
    (set-face-foreground 'qt-keywords-face "BlueViolet")
    ;; qt keywords
    (font-lock-add-keywords 'c++-mode
                            '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
    )
  (setq c-basic-offset 4 tab-width 4 indent-tabs-mode nil)
  (hs-minor-mode 1)
  (hide-ifdef-mode 0)
  (setq hide-ifdef-lines t)   			; if, ifdef 표시하지 않기
  (local-set-key "\C-c\C-c" 'compile)
  ;(local-set-key [(control return)] 'semantic-complete-analyze-inline)  ; control-enter를 cedet의 complete로 map
  (doxymacs-mode)
  (turn-on-eldoc-mode)
  )

;; .h, .c 도 c++ 모드로
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
				("\\.c$" . c++-mode))
				auto-mode-alist)) 

; previous error & next error
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)

;; Eldoc mode enable
; (autoload 'turn-on-eldoc-mode "eldoc" nil t)
; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
; (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
; (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; load c-eldoc mode
; (autoload 'turn-on-eldoc-mode "c-eldoc" nil t)
