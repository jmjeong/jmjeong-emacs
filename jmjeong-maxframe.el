;; maxframe.el [2009-10-22]
(require 'maxframe)
;; (add-hook 'window-setup-hook 'maximize-frame t)

(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (restore-frame)
	(maximize-frame)))

;; M-return을 semantic-complete-analyze-inline로 사용함에 따라 변경
(global-set-key (kbd "<C-M-return>") 'my-toggle-fullscreen)


;; full screen mode. in linux and windows
;;
;; (defvar my-fullscreen-p t "Check if fullscreen is on or off")

;; (defun my-non-fullscreen ()
;;   (interactive)
;;   (if (fboundp 'w32-send-sys-command)
;; 	  ;; WM_SYSCOMMAND restore #xf120
;; 	  (w32-send-sys-command 61728)
;; 	(progn (set-frame-parameter nil 'width 82)
;; 		   (set-frame-parameter nil 'fullscreen 'fullheight))))

;; (defun my-fullscreen ()
;;   (interactive)
;;   (if (fboundp 'w32-send-sys-command)
;; 	  ;; WM_SYSCOMMAND maximaze #xf030
;; 	  (w32-send-sys-command 61488)
;; 	(set-frame-parameter nil 'fullscreen 'fullboth)))

;; (defun my-toggle-fullscreen ()
;;   (interactive)
;;   (setq my-fullscreen-p (not my-fullscreen-p))
;;   (if my-fullscreen-p
;; 	  (my-non-fullscreen)
;; 	(my-fullscreen)))
