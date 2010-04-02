(require 'cl)
;; C-z를 사용할 일이 없으니까 재정의 해서 유용하게 사용하자
(global-unset-key "\C-z")
(global-set-key "\C-z" (make-keymap))
(loop for key in
      `(("c" . calendar)
		("C-c" . calendar)
		("x" . jm-switch-to-svn-status-buffer)
		("g" . gnus)
		("C-x" . git-status)
		("m" . mew)
        ("z" . other-frame)
        ("C-z" . other-frame)
		("b" . bbdb)
		("C-v" . sourcepair-load))
      do (define-key global-map
           (read-kbd-macro (concat "C-z " (car key))) (cdr key)))

;; C-x k를 재정의
(global-set-key "\C-xk" 'kill-this-buffer)

;; C-x z(C-z)
(global-set-key "\C-xz" 'other-frame)
(global-set-key "\C-xC-z" 'other-frame)


;; find-tag에 관한 재정의
(fset 'find-next-tag "\C-u\256")        ; macro for C-u M-.
(fset 'find-prev-tag "\C-u-\256")       ; macro for C-u - M-.
(global-set-key "\M-]" 'find-next-tag)
(global-set-key "\M-[" 'find-prev-tag)

;; C-m을 newline-and-indent로 재정의
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key [(control tab)] 'jm-other-window-or-other-buffer)
(global-set-key [(control ?`)] 'jm-other-window-or-other-buffer)

;; (global-set-key "\M-/" 'hippie-expand)

;; alt key 사용대신 C-xC-n을(원래는 set-goal-column)
;; Invoke M-x without the Alt key
(global-set-key "\C-x\C-n" 'execute-extended-command)
(global-set-key "\C-c\C-n" 'execute-extended-command)

;; Prefer backward-kill-word over Backspace
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
