(autoload 'file-template-auto-insert "file-template" nil t)
(autoload 'file-template-find-file-not-found-hook "file-template" nil t)
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)

;; (require 'file-template)

;; (setq file-template-paths "~/my-dot-elisp/template")
(setq file-template-paths (concat elisp-root-dir "/template"))
(setq file-template-insert-automatically 'ask)

;; (setq file-template-mapping-alist
;; 	  (cons  '("\\.m$" . "template.m") file-template-mapping-alist))

(setq file-template-mapping-alist
  '(
    ("\\.h\\(pp\\)?$" . "template.h")
    ("\\.m$" . "template.m")
    ("\\.py$" . "template.py")))
