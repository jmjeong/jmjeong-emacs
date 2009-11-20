;; android-mode.el [2009-10-30]
(require 'android-mode)
(setq android-mode-sdk-dir "~/android/platforms/android-2.0/")

;; emdroid.el [2009-09-11]
(require 'emdroid)
(setq emdroid-tools-dir "~/android/tools")

;; load jdee environment for Android
;;
;; (autoload 'jde-mode "jde" "JDE mode." t)
;; (setq auto-mode-alist
;;       (append '(("\\.java\\'" . jde-mode))  auto-mode-alist)

;; Setup Emacs to run bash as its primary shell.
;; (setq shell-file-name "bash")
;; (setq shell-command-switch "-c")
;; (setq explicit-shell-file-name shell-file-name)
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-sh-args '("-login" "-i"))