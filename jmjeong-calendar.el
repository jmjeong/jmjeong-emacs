;; calendar korean lunar [2007-12-07] jmjeong
(if (>= emacs-major-version 23)
	(require 'cal-korean)
  (require 'cal-korea22))

;; 음력 표시할 때 길게 표시
(eval-after-load "cal-korean"
  '(progn
	 (setq calendar-korean-print-long-description t)
	 ))

;; diary mode
(setq view-diary-entries-initially t
       mark-diary-entries-in-calendar t
       number-of-diary-entries 7)
(add-hook 'diary-display-hook 'diary-fancy-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(add-hook 'fancy-diary-display-mode-hook
	   '(lambda ()
              (alt-clean-equal-signs)))

(defun alt-clean-equal-signs ()
  "This function makes lines of = signs invisible."
  (goto-char (point-min))
  (let ((state buffer-read-only))
	(when state (setq buffer-read-only nil))
	(while (not (eobp))
	  (search-forward-regexp "^=+$" nil 'move)
	  (add-text-properties (match-beginning 0) 
						   (match-end 0) 
						   '(invisible t)))
	(when state (setq buffer-read-only t))))

(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diary ((t (:foreground "darkred")))))

;; diary file entry
;; 
(setq diary-file "~/.emacs.d/emacs-diary")

