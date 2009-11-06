(require 'tempbuf)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'inferior-python-mode-hook 'turn-on-tempbuf-mode)

(setq tempbuf-kill-message nil)
(and (fboundp 'temp-buffer-resize-mode) (temp-buffer-resize-mode t)) ; temp-buffer의 window는 작게
