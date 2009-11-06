(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
 
;; Optional setup (Read Mail menu for Emacs 21):
(if (boundp 'read-mail-command)
     (setq read-mail-command 'mew))
 
;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
     (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
     (define-mail-user-agent
       'mew-user-agent
       'mew-user-agent-compose
       'mew-draft-send-message
       'mew-draft-kill
       'mew-send-hook))
 
(setq mew-name "Jaemok Jeong") ;; (user-full-name)
(setq mew-user "jmjeong")      ;; (user-login-name)
(setq mew-mail-domain "nemustech.com")

(setq mew-pop-size 0)
;;(setq mew-imap-prefix-list '("#mh/" "#mhinbox"))
;;(setq mew-auto-get t)
(setq toolbar-mail-reader 'Mew)
(setq mew-use-cached-passwd t)
(setq mew-passwd-timer-unit 999)
(setq mew-passwd-lifetime 999)
(set-default 'mew-decode-quoted 't)  
(setq mew-prog-pgp "gpg")
(setq mew-pop-delete nil)
;; (setq mew-config-alist 
;; ;;Gmail
;; 	'(
;; 	;;   ("default"
;; 	;; ("name"		. "Jaemok Jeong")
;; 	;; ("user"		. "jmjeong")
;; 	;; ("mail-domain"	. "gmail.com")
;; 	;; ("proto"	. "+")
;; 	;; ("pop-ssl"	. t)
;; 	;; ("pop-ssl-port"	. "995")
;; 	;; ("prog-ssl"	. "/opt/local/bin/stunnel")
;; 	;; ("pop-auth"	. pass)
;; 	;; ("pop-user"	. "jmjeong")
;; 	;; ("pop-server"	. "pop.gmail.com")
;; 	;; ("smtp-ssl"	. t)
;; 	;; ("smtp-ssl-port". "465")
;; 	;; ("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5"))
;; 	;; ("smtp-user"	. "jmjeong")
;; 	;; ("smtp-server"	. "smtp.gmail.com")
;; 	;; )
;;   ("IMAP"
;; 	("name"	. "Jaemok Jeong")
;; 	("user"	. "jmjeong")
;; 	("mail-domain" . "mail.nemustech.com")
;; 	("proto" . "%")
;; 	("imap-server"	. "mail.nemustech.com")
;; 	("imap-user"	. "jmjeong")
;; 	("imap-size"	. 0)
;; 	("imap-ssl"     . t)
;; 	("imap-auth-list" (quote ("PLAIN" "LOGIN")))
;; 	("imap-ssl-port" . 993)
;; 	;; ("smtp-ssl"	. t)
;; 	;; ("smtp-ssl-port". "465")
;; 	;; ("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5"))
;; 	;; ("smtp-user"	. "jmjeong")
;; 	;; ("smtp-server"	. "mail.nemustech.com")
;; 	;; ("imap-delete" . t)
;; 	;; ("imap-queue-folder" . "%queue") 
;; 	;; ("imap-trash-folder" . "%INBOX.Trash") ;; This must be in concile with your IMAP box setup
;; 	)
;;  ))

(setq mew-proto "%")

(setq mew-ssl-verify-level 0)
(setq mew-imap-server "nemustech.com")
(setq mew-imap-ssl t)
(setq mew-imap-ssl-port 993)
(setq mew-prog-ssl "/opt/local/bin/stunnel")
(setq mew-prog-mewl "/usr/local/bin/mewl")

(setq mew-summary-form '(type (5 date) " " (14 from) " " t (70 subj) ))

