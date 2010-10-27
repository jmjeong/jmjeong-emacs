(setq user-mail-address "jmjeong@gmail.com")
(setq user-full-name "Jaemok Jeong")

(setq mail-user-agent 'sendmail-user-agent)

(setq gnus-select-method '(nnimap "nemustech"
    (nnimap-address "mail.nemustech.com")
    (nnimap-server-port 993)
    ; 로그인 정보 파일을 사용하여 자동 로그인 하고자 할 경우 사용
	(nnimap-authinfo-file "~/.imap-authinfo")	
    (nnimap-stream ssl)))
	
;; .imap-authinfo file format 
;;  
;;  machine imap.gmail.com login MyEmailAddress password MyPassWord port 993
;;
(setq gnus-show-threads t)
(setq gnus-thread-indent-level 2)

(setq gnus-fetch-old-headers t)

;; for gnus speed-up
(setq gc-cons-threshold 12000000)		; 35M -> 12M
(setq gnus-use-correct-string-widths nil)
(require 'message)
(add-to-list 'message-syntax-checks '(sender . disabled))
(eval-after-load "message"
      '(add-to-list 'message-syntax-checks '(sender . disabled)))

(setq imap-use-utf7 nil)

;; [2009-10-29] 보낸 메일들을 imap에 있는 Sent foler에 저장
;;
(setq message-default-headers "Bcc: jmjeong@nemustech.com")
;; (setq message-default-headers "Newsgroups: INBOX.Sent")

;(add-hook 'message-send-hook 'ispell-message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-check-zip-codes-p nil)

(setq message-mode-hook
      (quote (
      (lambda nil (setq fill-column 72))
	  turn-on-auto-fill
	  bbdb-define-all-aliases
	  ; (lambda () (variable-pitch-mode nil)) 
	  ;; (lambda nil (if window-system
	  ;; 						 (set-face-font 'frame "-apple-NanumGothic-medium-normal-normal-*-14-*-*-*-m-0-*-*")))
	  )))

;; C-M-g를 gnus로 재정의
(global-set-key "\C-\M-g" 'gnus)
