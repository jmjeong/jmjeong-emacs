
;; TAGS 파일 생성용 script [2006-07-23]
;;    asynchronos하게 script를 수행
(defun jm-create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (start-process-shell-command "create-tags" "*create-tags*"
							   (format "find %s -type f \( -name '*.c' -o -name '*.cpp' -o -name '*.h' -o -name '*.m' \) | etags - -o %s/TAGS" dir-name dir-name))
  (set-process-sentinel (get-process "create-tags")  #'create-tags-sentinel))

(defun create-tags-sentinel (process event)
  (when (member (process-status process) '(closed exit))
	(save-excursion
	  (unwind-protect
	    (progn
		  (kill-buffer "*create-tags*")
		  (message "Create tags finised"))))))


;; naver에서 영문 단어 검색 [2006-07-24]
;;    나오는 메시지 좀 정렬해서 깔끔 버젼으로 만들어야지.
(defun jm-ndic (word)
  "search WORD in endic.naver.com"
  (interactive
   (list (let* ((wd (current-word))
				(word (read-string 
					   (format "Dict what (default `%s'): " wd))))
		   (if (string= "" word) wd word))))
  (browse-url (concat "http://dic.naver.com/search.naver?query=" word)))

(defun google-search (word)
  "Mostly copied from `w3m-dict-cn-search'."
  (interactive
   (let ((v (current-word))
	 val)
     (setq val
	   (read-from-minibuffer
	    (if (stringp v)
		(format "Search on google (default %s): " v)
	      "Search on google: ")))
     (list (if (equal val "")
	       v
	     val))))
  (let ((url (format
	      "http://www.google.com/#hl=en&q=%s" word)))
    ;;TODO process started in this way will be terminated when emacs exit
    ;;     I want to start a process like M-! foo &
    ;;     don't start as sub-process, but a independent process from emacs.
    (start-process "google-search" nil " /Applications/Firefox.app/Contents/MacOS/firefox" url)))

;; mac version-itunes (current song)
;; [2009-04-03]

(defun current-itunes-song ()
  (interactive)
  (message 
   (do-applescript
	"tell application \"iTunes\"
           set currentTrack to the current track
           set artist_name to the artist of currentTrack
           set song_title to the name of currentTrack
           return artist_name & \" - \" & song_title
        end tell")))

(defun jm-switch-to-svn-status-buffer ()
  "if there is a buffer named *svn-status*, switch to it. or execute svn-status"
  (interactive)
  (if (remove-if-not #'(lambda (x) (string-equal "*svn-status*" x))
                     (mapcar #'buffer-name (buffer-list)))
      (switch-to-buffer "*svn-status*")
    (call-interactively 'svn-status)))


(defun jm-switch-to-cvs-status-buffer ()
  "if there is a buffer named *svn-status*, switch to it. or execute svn-status"
  (interactive)
  (if (remove-if-not #'(lambda (x) (string-equal "*cvs*" x))
                     (mapcar #'buffer-name (buffer-list)))
      (switch-to-buffer "*cvs*")
    (call-interactively 'cvs-examine)))

(defun jm-other-window-or-other-buffer ()
  "Select other window on this frame or, if none, switch to `other-buffer'."
  (interactive)
  (if (one-window-p)
      (switch-to-buffer (other-buffer))
    (other-window +1)))

(defun add-current-to-load-path (dir)
  "Add current directory into load path"
  (interactive "DPath:")
  (add-to-list 'load-path dir))


(setq Info-fontify-maximum-menu-size (* 128 1024))
(setq Info-enable-edit t)
(setq auto-save-timeout 180)
(setq auto-save-list-file-prefix "~/.autosave/saves-")

;; tramp 동작할때는 auto save 끄기 [2010-03-04 Thu]
(add-to-list 'backup-directory-alist
			 (cons tramp-file-name-regexp nil))

(setq three-step-help t)	   ;
(delete-selection-mode 1) ; 윈도우처럼, 선택된 regeion 을 DEL 로 지우거나, 다른 글자를 타이핑 할때 즉시 지운다.
;; (which-function-mode 1)					; 어떤 함수를 수정중인지 표현 

;; Backup 파일을 .backups directory에 만듦(backups가 없으면, 뒤에 ~를 붙임)
;;    (replace built-in make-backup-file-name function)
;;
(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'."
  (require 'dired)
  (if (file-exists-p "~/.backups")
      (concat (expand-file-name "~/.backups/")
              (dired-replace-in-string "/" "|" file-name))
    (concat file-name "~")))

;; % 이동
(define-key global-map (kbd "%") 'match-paren)
(defun match-paren ()
  "% command of vi"
  (interactive)
  (let ((char (char-after (point))))
    (cond ((memq char '(?\( ?\{ ?\[))
           (forward-sexp 1)
           (backward-char 1))
          ((memq char '(?\) ?\} ?\]))
           (forward-char 1)
           (backward-sexp 1))
          (t (call-interactively 'self-insert-command)))))


(defun jm-other-window-or-other-buffer ()
  "Select other window on this frame or, if none, switch to `other-buffer'."
  (interactive)
  (if (one-window-p)
      (switch-to-buffer (other-buffer))
    (other-window +1)))

(defun jm-byte-compile-this-buffer()
  "Byte compile this buffer"
  (interactive)
  (save-buffer)
  (byte-compile-file (buffer-file-name)))

;; insert-path macro
(defun insert-path (file)
  "Insert a path into the buffer with completion"
  (interactive "GPath: ")
  (insert (expand-file-name file)))


(global-set-key (kbd "C-x C-i") 'insert-path)

;; insert date into buffer at point
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
;;
;; C-u prefix와 함께 수행하면 시간도 같이 입력
;;
(defun insert-date (arg)
  "Insert date at point."
  (interactive "P")
  (if (not arg)
	  (insert (format-time-string "[%Y-%m-%d %a]"))
	(insert (format-time-string "[%Y-%m-%d %a %p %l:%M]"))	
	))

(global-set-key (kbd "C-c , d") 'insert-date)

;; el 파일을 save할 때, 이미 elc 파일이 있는 경우 byte compile하도록
;; 
;; (unless noninteractive
;;   (defvar maybe-byte-compile-file t
;;     "If non-nil, byte-compiles el file if it was before when saved.")
;;   (defun maybe-byte-compile-file
;; 	"For files ending in \".el\", byte-compile if there is a \".elc\" file.
;; Also, if the file appears to be loaded, re-load it."
;;     (when (and maybe-byte-compile-file
;; 			   (string= (substring buffer-file-name -3) ".el")
;; 			   (file-exists-p (concat buffer-file-name "c")))
;;       (let ((reload
;; 			 (save-excursion
;; 			   (widen)
;; 			   (goto-char (point-min))
;; 			   (and (search-forward "\n(provide '" nil t)
;; 					(featurep (read (current-buffer)))))))
;; 		(byte-compile-file buffer-file-name reload))))
;;   (add-hook 'after-save-hook #'maybe-byte-compile-file t))

;; Source pair(C-z, C-v에 매핑)
(load "sourcepair")
(setq sourcepair-source-extensions (quote (".cpp" ".cxx" ".cc" ".C" ".c" ".m")))

;; Desktop save mode
; (add-to-list 'desktop-globals-to-save 'file-name-history)  
; (setq history-length 250)
; (desktop-save-mode 1)
; (desktop-read)


;; tabbar mode [2009-04-08]
;(load "tabbar" nil t)

;; (load "jmjeong-devenv" nil t)
