;; C-mode setting
;; 
;; doxymacs font lock setting
;;
(autoload 'doxymacs-font-lock "doxymacs" nil t)
(autoload 'doxymacs-mode "doxymacs" nil t)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
