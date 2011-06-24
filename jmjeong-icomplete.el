;; 
;; Turn on iswitch mode
(iswitchb-mode 1)

;(resize-minibuffer-mode t)
(add-hook 'icomplete-minibuffer-setup-hook 'icomplete-setter)
(add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)

(icomplete-mode t)		   ; incremental minibuffer completion


(defun icomplete-setter ()
  (make-local-variable 'resize-minibuffer-window-max-height)
  (setq resize-minibuffer-window-max-height 3))
(add-hook 'minibuffer-setup-hook 'minibuffer-setter)
(defun minibuffer-setter ()
  (setq truncate-lines nil))
;; this is replacement of iswitchb-default-keybindings
;(iswitchb-default-keybindings)
(add-hook 'iswitchb-define-mode-map-hook
          'iswitchb-my-keys)
(defun iswitchb-my-keys ()
  "Add my keybindings for iswitchb."
  (define-key iswitchb-mode-map " " 'iswitchb-next-match)
)

;; (add-hook 'completion-list-mode-hook 'my-text-decrease)
;; (defun my-text-decrease ()
;;   (set-frame-font "Monaco"))
  
(global-set-key (kbd "C-x b")  'iswitchb-buffer)
(global-set-key (kbd "C-x 4 b")  'iswitchb-buffer-other-window)
(global-set-key (kbd "C-x 4 C-o")  'iswitchb-display-buffer)
(global-set-key (kbd "C-x 5 b")  'iswitchb-buffer-other-frame)
