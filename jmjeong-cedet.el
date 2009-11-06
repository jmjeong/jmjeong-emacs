;; --------------------------------------------------------------------------------
;; cedet [2009-10-30]
;;    Emacs devel version에 내장되어 있는 cedet는 불안정해서, CVS cedet을 다시 인스톨
;;
;; See cedet/common/cedet.info for configuration details.

(require 'cedet)

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;;(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;;(semantic-load-enable-gaudy-code-helpers)
(global-set-key [(meta return)] 'semantic-complete-analyze-inline)

;; Block confusing display
;; (global-semantic-stickyfunc-mode)

;; * This enables the use 
;; of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

;; Eassist.el [2009-10-30]
(when window-system
    (require 'eassist)
    (defun my-c-mode-common-hook ()
      (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
      (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
	(add-hook 'java-mode-common-hook 'my-c-mode-common-hook))
