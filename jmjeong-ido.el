(setq ido-confirm-unique-completion t)
(setq ido-default-buffer-method 'samewindow)
(setq ido-use-filename-at-point t)

(ido-mode t)
(ido-everywhere t)

(set-face-background 'ido-first-match "white")
(set-face-foreground 'ido-subdir "blue3")

(icomplete-mode 1)
