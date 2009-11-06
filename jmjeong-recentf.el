;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-save-list 50)
(setq recentf-max-menu-items 20)

;; anything을 통해 접근하기 때문에 keybinding을 제거
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

