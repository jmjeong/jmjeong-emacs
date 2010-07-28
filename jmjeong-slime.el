;; cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime 
(add-to-list 'load-path "~/svn/slime/")  ; your SLIME directory
(require 'slime)
;; UTF-8 인코딩을 기본으로 사용 --> 한글 symbol 사용을 위해
(setq slime-net-coding-system 'utf-8-unix)
;; Lisp 실행 파일
(setq inferior-lisp-program "/opt/local/bin/sbcl") ; your Lisp system

(add-hook 'inferior-lisp-mode-hook
    (lambda () (inferior-slime-mode t)))
(add-hook 'lisp-mode-hook
    (lambda ()
            (set (make-local-variable
                  'lisp-indent-function)
                 'common-lisp-indent-function)))
(slime-setup)
