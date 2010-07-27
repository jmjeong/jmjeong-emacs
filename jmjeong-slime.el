(setq inferior-lisp-program "/opt/local/bin/sbcl") ; your Lisp system
;; cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime 
(add-to-list 'load-path "~/svn/slime/")  ; your SLIME directory
(require 'slime)
(slime-setup)

