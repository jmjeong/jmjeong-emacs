;; My passwords are obviously not here. These are for you to override yourself.

; Twitter
;(setq twit-user "jmjeong")
;(setq twit-pass "YourTwitterPassword")

;; twittering mode
(setq twittering-username "jmjeong")
;(setq twittering-password "twitter password here") ; This is optional

;; Gmail
;; (setq gmail-user "jmjeong")
;; (setq gmail-pass "YourGmailPassword")

;; gist setting
(setq github-username "jmjeong")
; (setq github-api-key "")

;; You can also do as I do and keep them in a seperate folder:
(add-to-list 'load-path "~/.emacs.private")
;;Load the password file but don't complain if it doesn't exist
(load "emacs-real-passwords" 'noerror)
