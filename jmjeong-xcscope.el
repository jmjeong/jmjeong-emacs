(require 'xcscope)
(setq cscope-do-not-update-database t)

(add-hook 'objc-mode-hook (function cscope:hook))

