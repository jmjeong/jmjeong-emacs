; twittering mode [2009-07-30]
(require 'twittering-mode)

(setq twittering-username "jmjeong")
(global-set-key "\C-xt" 'twittering-mode)

; (setq twittering-password "twitter password here") ; This is optional

;; [2009-10-26] twittering-reply-to-user가 왜 hotkey로 지정이 안 되었나 싶었는데, 그냥 ENTER치니
;; 이 기능으로 동작한다. 삭제
;;
;; (if twittering-mode-map
;;     (let ((km twittering-mode-map))
;;       (define-key km "\C-cr" 'twittering-reply-to-user)))


