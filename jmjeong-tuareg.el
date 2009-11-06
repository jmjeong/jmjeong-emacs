;(autoload 'tuareg-imenu-set-imenu "tuareg-imenu" 
;  "Configuration of imenu for tuareg" t)
;(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist 
	  (append '(("\\.ml[ily]?$" . tuareg-mode)
				("\\.topml$" . tuareg-mode))
			  auto-mode-alist))