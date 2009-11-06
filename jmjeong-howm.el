;;
;; Howm-mode setting [2006-07-17 22:01]
;;
(require 'howm)
(setq howm-directory (concat orghome "/journal/howm/"))
(setq howm-history-file (concat orghome "/journal/.howm-history"))
(setq howm-keyword-file (concat orghome "/journal/.howm-keys"))
(setq howm-menu-file "howm-menu")

(global-set-key (kbd "C-= C-\\") 'howm-menu)
