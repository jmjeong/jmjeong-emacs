;; auto-complete mode

(require 'auto-complete)
; (global-auto-complete-mode t)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; (setq ac-auto-start nil)
;; (global-set-key "\M-/" 'ac-start)

(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" 'ac-complete)

(setq ac-auto-start 2)
(define-key ac-complete-mode-map "\M-/" 'ac-stop)