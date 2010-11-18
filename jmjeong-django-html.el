(autoload 'django-html-mode "django-html-mode")
(add-to-list 'auto-mode-alist '("\\.[sx]?html?\\'" . django-html-mode))

(defun start-django-shell()
  (interactive)
  (ansi-term "/usr/local/bin/python ../manage.py shell")
  )
