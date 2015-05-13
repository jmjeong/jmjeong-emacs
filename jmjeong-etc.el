(autoload 'rainbow-mode "rainbow-mode" nil t)
(require 'js2-mode)
(require 'coffee-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (setq tern-ac-on-dot t)
     (tern-ac-setup)))
