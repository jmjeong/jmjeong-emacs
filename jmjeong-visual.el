;; inhibit the startup screen
(setq inhibit-startup-message t)

; (set-cursor-color 'red)
(setq default-frame-alist
	  ' ((cursor-color . "red") (cursor-type . box) (width . 60) (height . 55)))

;; 모드 라인에 Display time mode 활성화
(display-time-mode t)
(setq display-time-day-and-date t)

; ediff configuration [2009-03-05]
(setq ediff-merge-split-window-function (quote split-window-horizontally))
(setq ediff-split-window-function (quote split-window-horizontally))

;; Fine tuning 
;; 
(standard-display-ascii ?\13 "")	; C-m 은 표시하지 않도록 함
; (setq-default line-spacing 1)		; line spacing 조정
(show-paren-mode t)			; 괄호 보이게 하는 모드
;; default 값은 이유가 있어서 default 값이라고 함 - 세밀한 제어를 위해 이것이 필요하다고 함
;;(setq kill-whole-line t)							 ; C-k가 한줄 전체 지우도록
(setq default-tab-width 4)							 ; tab width 설정
(setq-default fill-column 100)					     ; 
(fset 'yes-or-no-p 'y-or-n-p)					     ; y, n으로 질문 대답
(setq enable-local-eval 'ask)					     ; local evaluation
(setq enable-local-variables t)					     ; local variable 적용
(auto-compression-mode t)							 ; auto-compression
(setq max-lisp-eval-depth 3000)
(setq max-specpdl-size 3000)
(setq-default filladapt-mode t)

; [2009-07-21]
;; (if window-system
;; 	(set-face-font 'default "-apple-NanumGothicCoding-medium-normal-normal-*-14-*-*-*-m-0-*-*"))