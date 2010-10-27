(require 'calendar)
(require 'org)
(require 'org-install)

;; [2007-04-03] Windows면 설정 directory를 d:/workspace에서 읽고, 아니면 홈 directory에서 읽도록
;;
;(if win32p (setq orghome "g:/workspace")
;  (setq orghome "~/workspace"))
(defvar orghome "~/workspace")

;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(setq org-cycle-include-plain-lists t)
(setq org-log-done t)
(add-hook 'mail-mode-hook 'turn-on-orgstruct)

;; Planner 관련 자주쓰는 key 할당
;;
;; Some keyboard shortcuts
;;
(global-unset-key (kbd "C-="))
;;(global-set-key (kbd "C-= C-=") 'jm-open-planner-file-current)
;;(global-set-key (kbd "C-= C--") 'jm-open-planner-file-previous)
(global-set-key (kbd "C-= C-=") 'jm-open-org-file-current)
(global-set-key (kbd "C-= C--") 'jm-open-org-file-previous)

; (global-set-key (kbd "C-= c") 'planner-create-task-from-buffer)
; (global-set-key (kbd "C-= t") 'planner-goto-today)

; (global-set-key (kbd "C-= T") 'my-task-pool)
; (global-set-key (kbd "C-= j") 'my-jproject-pool)
; (global-set-key (kbd "C-= P") 'planner-goto)
; (global-set-key (kbd "C-c t") 'planner-insert-current-time)

;; org mode에서 emphasis에 multibyte는 적용받지 않도록 함
(eval-after-load "org"
  '(setq  org-emphasis-regexp-components
		  '(" \t('\"{" 
			"- \t.,:!?;'\")}[:multibyte:]" 
			" \t\r\n,\"'" 
			"." 
			1)))

(add-hook 'org-mode-hook
		  (let ((original-command (lookup-key org-mode-map [tab])))
			`(lambda ()
			   (setq yas/fallback-behavior
					 '(apply ,original-command))
			   (auto-fill-mode)
			   (local-set-key [tab] 'yas/expand))))

;; (add-hook 'org-mode-hook
;; 		  (lambda ()
;; 			(org-set-local 'yas/trigger-key  (kbd "TAB"))
;; 			(define-key yas/keymap [tab] 'yas/next-field-group)
;; 			(auto-fill-mode)
;; 			(add-hook 'local-write-file-hooks 'blog-updated-timestamp)
;; 			))

(defun blog-updated-timestamp ()
  "Upate blog-updated-timestamp"
  (save-excursion
	(goto-char (point-min))
	(let ((state buffer-read-only))
	  (when state (setq buffer-read-only nil))
	  (if (search-forward-regexp "^updated:" nil t)
		  (let ((beg (match-end 0)))
			(end-of-line)
			(delete-region beg (point))
			(goto-char (match-end 0))
			(kill-line)
			(insert (format-time-string " %Y/%02m/%02d %02H:%02M:%02S\n" (current-time)))
			)
		)
	  (when state (setq buffer-read-only t)))))

(setq org-agenda-directory (concat orghome "/journal/org/"))

(defun jm-open-org-file (week)
  "Open Current Orb-mode file based on current date."
  (interactive)
  (save-excursion
    (let* ((cur-date (calendar-current-date))
           (abs-day (calendar-absolute-from-gregorian cur-date))
           (day (calendar-day-of-week
                 (calendar-gregorian-from-absolute abs-day)))
           last-abs-day
           first-week-day-struct
		   last-week-day-struct
           jm-buffer-name
           )
    ;; search first day
    (while (not (= day calendar-week-start-day))
      (setq abs-day (1- abs-day))
      (setq day (calendar-day-of-week
                 (calendar-gregorian-from-absolute abs-day))))
    (setq abs-day (+ 1 abs-day))
    (setq abs-day (+ abs-day (* 7 week)))
    (setq last-abs-day (+ 4 abs-day))
    (setq first-week-day-struct (calendar-gregorian-from-absolute abs-day))
    (setq last-week-day-strunct (calendar-gregorian-from-absolute last-abs-day))
    (setq jm-buffer-name (format "%s%02d%02d~%s%02d%02d"
                         (substring (int-to-string (third first-week-day-struct)) 2 4)
                           (first first-week-day-struct)
                          (second first-week-day-struct)
                         (substring (int-to-string (third last-week-day-strunct)) 2 4)
                           (first last-week-day-strunct) 
                          (second last-week-day-strunct)
                          ))
    (find-file (concat org-agenda-directory "/" jm-buffer-name ".org"))

;    (switch-to-buffer (get-buffer-create jm-buffer-name))
;    (erase-buffer)
;    (insert-file-contents (concat "f:/workspace/journal/Plans/" jm-buffer-name))
    )
  )
)

(defun jm-open-org-file-current ()
  "Open Current planer"
  (interactive)
  (jm-open-org-file 0))

(defun jm-open-org-file-previous ()
  "Open Current planer"
  (interactive)
  (jm-open-org-file -1))

;; (defun jmjeong-org-directory-file (dir)
;;   "Private org directory file lister"
;;   (let ((contents (directory-files dir))
;; 		expanded result)
;; 	(progn
;; 	  (while contents
;; 		(setq expanded (expand-file-name (concat dir (car contents))))
;; 		(if (and (file-regular-p expanded)
;; 				   (string-match "org$" expanded))
;; 			(setq result (nconc result (list expanded))))
;; 		(setq contents (cdr contents))))
;; 	result
;; 	))
;; (setq org-agenda-files (jmjeong-org-directory-file (concat orghome "/journal/org/")))


;; Org-mode wiki 페이지에서 다음과 같은 scipt를 보게 됨. directory-files가 option이 있었군..
;;
;;   [2009-10-15] Org 파일을 company, personal로 분리하여 directory로 관리 함에 따라 해당내용 읽어오도록 변경
(defun jm-rescan-org-agenda-files ()
  "Rescan org agenda files for private org directory"
  (interactive)
  (setq org-agenda-files
		(append ;(directory-files (expand-file-name org-agenda-directory) t "^070[8-9].*\\.org$")
		;(directory-files (expand-file-name org-agenda-directory) t "^071[0-2].*\\.org$")
		 (directory-files (expand-file-name org-agenda-directory) t "^[^0].*\\.org$")
		 (directory-files (expand-file-name (concat org-agenda-directory "company")) t "\\.org$")
		 (directory-files (expand-file-name (concat org-agenda-directory "personal")) t "\\.org$"))))

;; (jm-rescan-org-agenda-files)
(org-remember-insinuate)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

; remember keymap
(define-key global-map [(control meta ?r)] 'remember)
(global-set-key "\C-cr" 'remember)

(custom-set-variables
 '(org-default-notes-file (concat org-agenda-directory "/notes.org"))
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
;;  '(org-agenda-custom-commands
;;    (quote (("d" todo "DELEGATED" nil)
;;            ("c" todo "DONE|DEFERRED|CANCELLED" nil)
;;            ("w" todo "WAITING" nil)
;;            ("W" agenda "" ((setq org-agenda-ndays 7)))
;; 		   ("A" agenda ""
;; 			((org-agenda-skip-function
;; 			  (lambda nil
;; 				(org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
;;              (org-agenda-ndays 1)
;; 			 (org-agenda-overriding-header "Today's Priority #A tasks: ")))
;; 		   ("u" alltodo ""
;; 			((org-agenda-skip-function
;; 			  (lambda nil
;; 				(org-agenda-skip-entry-if (quote scheduled) (quote deadline)
;; 										  (quote regexp) "<[^>\n]+>")))
;; 			 (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   `((?n "* %u %?" ,(concat org-agenda-directory "notes.org") "Notes")
	 (?t "* TODO %?\n  %u" ,(concat org-agenda-directory "todo.org") "Tasks"))))

;; (setq org-blank-before-new-entry  '((heading . t) (plain-list-item . nil)))

(setq org-return-follows-link t)
(setq org-tab-follows-link t)

;; (setq org-hide-leading-stars nil)

(setq org-show-hierarchy-above '(default . nil))
(setq org-show-following-heading '(default . nil))

(setq org-startup-folded nil)

;; diary entry 포함
(setq org-agenda-include-diary t)

;; todo의 keyword 추가
(setq org-todo-keywords
	  '((sequence "TODO(t)" "DELEGATED(l)" "MAYBE(m)" "|"  "DONE(d)" "CANCELLED(x)")))

(setq org-tag-alist '(("mac" . ?m) ("emacs" . ?e) ("hobby" . ?h) ("iphone" . ?i) ("python" . ?p)))

(setq org-todo-keyword-faces
	  '(("TODO"      . org-warning)
 		("DELEGATED"  . shadow)
		("DONE"  . shadow)
		("CANCELED"  . (:foreground "blue" :weight normal))))

(setq org-agenda-custom-commands
	  '(("d" todo "WAITING|DELEGATED" nil)
		("c" todo "DONE|CANCELLED|MAYBE" nil)))

;; for MacTexs
(setq org-export-latex-default-class "article")

;; [2007-10-08]
;;
;; (eval-after-load "org"
;;   '(progn
;;      (define-prefix-command 'org-todo-state-map)

;;      (define-key org-mode-map "\C-cx" 'org-todo-state-map)

;;      (define-key org-todo-state-map "x"
;;        #'(lambda nil (interactive) (org-todo "CANCELLED")))
;;      (define-key org-todo-state-map "d"
;;        #'(lambda nil (interactive) (org-todo "DONE")))
;;      (define-key org-todo-state-map "f"
;;        #'(lambda nil (interactive) (org-todo "DEFERRED")))
;;      (define-key org-todo-state-map "l"
;;        #'(lambda nil (interactive) (org-todo "DELEGATED")))
;; ;;      (define-key org-todo-state-map "s"
;; ;;        #'(lambda nil (interactive) (org-todo "STARTED")))
;;      (define-key org-todo-state-map "w"
;;        #'(lambda nil (interactive) (org-todo "WAITING")))

;;      (define-key org-agenda-mode-map "\C-n" 'next-line)
;;      (define-key org-agenda-keymap "\C-n" 'next-line)
;;      (define-key org-agenda-mode-map "\C-p" 'previous-line)
;;      (define-key org-agenda-keymap "\C-p" 'previous-line)))

;;
;; org-blog setting [2007-08-30]
;;
;; (require 'org-blog)
;; (setq org-blog-directory (concat orghome "/journal/org/blog"))
;; (setq org-blog-unfinished-directory (concat orghome "/journal/org/blog/unfinished"))
;; (setq org-blog-publish-directory (concat orghome "/journal/org/blog/html"))


;; body 부분만 생성해 내기 위해 org-publish.el에서 함수 가지고 와서 변경
;;   [2009-11-20 Fri] advice를 이용해서 간단히 할 수 있지 않을까???
(defun org-publish-org-to-local (format plist filename pub-dir)
  "Publish an org file to FORMAT.
PLIST is the property list for the given project.
FILENAME is the filename of the org file to be published.
PUB-DIR is the publishing directory."
  (require 'org)
  (unless (file-exists-p pub-dir)
    (make-directory pub-dir t))
  (let ((visiting (find-buffer-visiting filename)))
    (save-excursion
      (switch-to-buffer (or visiting (find-file filename)))
      (let* ((plist (cons :buffer-will-be-killed (cons t plist)))
	     (init-buf (current-buffer))
	     (init-point (point))
	     (init-buf-string (buffer-string))
	     export-buf-or-file)
	;; run hooks before exporting
	(run-hooks 'org-publish-before-export-hook)
	;; export the possibly modified buffer
	(setq export-buf-or-file
	      (funcall (intern (concat "org-export-as-" format))
		       (plist-get plist :headline-levels)
		       nil plist nil t pub-dir))
	(when (and (bufferp export-buf-or-file)
		   (buffer-live-p export-buf-or-file))
	  (set-buffer export-buf-or-file)
	  ;; run hooks after export and save export
	  (and (run-hooks 'org-publish-after-export-hook)
	       (if (buffer-modified-p) (save-buffer)))
	  (kill-buffer export-buf-or-file))
	;; maybe restore buffer's content
	(set-buffer init-buf)
	(when (buffer-modified-p init-buf)
	  (erase-buffer)
	  (insert init-buf-string)
	  (save-buffer)
	  (goto-char init-point))
	(unless visiting
	  (kill-buffer init-buf))))))

(defun org-publish-org-to-html-local (plist filename pub-dir)
  "Publish an org file to HTML.
See `org-publish-org-to' to the list of arguments."
  (org-publish-org-to-local "html" plist filename pub-dir))

(setq org-publish-project-alist
      '(
        ("blog-posts"
         :base-directory "~/workspace/journal/org/blog"
 		 :publishing-function org-publish-org-to-html-local
         :base-extension "org"
         :publishing-directory "~/t/blogofile.com/_tmp"
         :inline-images t
         :table-of-contents nil
         :drawers nil
         :todo-keywords nil ; Skip todo keywords
         :exclude "draft*" ; TODO fix
         :section-nuvmbers nil
		 :auto-index nil
         :auto-preamble nil
         :auto-postamble nil
		 :timestamp    t
		 :sub-superscript nil
		 :headline-levels 3
         )
        ))

;; (setq org-publish-project-alist
;; 	  '(("blog"
;; 		 :base-directory "~/workspace/journal/org/blog"
;; 		 :base-extension "org"
;; 		 :publishing-directory "d:/workspace/journal/org/blog/html"
;; 		 :exclude "PrivatePage.org"	;; regexp
;; 		 :headline-levels 3
;; 		 :section-numbers nil
;; 		 :table-of-contents nil
;; 		 :style "<link rel=stylesheet
;;                        href=\"../other/mystyle.css\" type=\"text/css\">"
;; 		 :auto-preamble t
;; 		 :auto-postamble nil)

;; 		("images"
;; 		 :base-directory "~/images/"
;; 		 :base-extension "jpg\\|gif\\|png"
;; 		 :publishing-directory "/ssh:user@host:~/html/images/"
;; 		 :publishing-function org-publish-attachment)

;; 		))

;; (defun write-blog ()
;;   ""
;;   (interactive)
;;   (dired "~/t/blogofile.com/_posts")
;; )
;; (global-set-key (kbd "<f5>") 'write-blog)
;;
;;
;; 간략화한 버젼
(global-set-key [f5] '(lambda () (interactive)(dired "~/workspace/journal/org/blog")))

;; 테스트용 blogofile build
;;
;;   blogofile은 static site generator tool이다
(global-set-key [f6] 'jm-generate-blogofile)

(defvar blogofile-output-buffer nil
  "The output buffer for blogofile")

;; [2009-10-30] 간략버젼
(defun jm-generate-blogofile ()
  (interactive)
  (org-publish-all)
  ;; (if (not (buffer-live-p blogofile-output-buffer))
  ;; 	  (setq blogofile-output-buffer (get-buffer-create "*blogofile output*")))
  ;; (pop-to-buffer blogofile-output-buffer)
  ;; (end-of-buffer)
  ;; (async-shell-command
  ;;  (format "cd %s;PYTHONPATH=~/workspace/blogofile:~/workspace/blogofile/blogofile python -m blogofile.main -vv build" "~/t/blogofile.com") nil)
  (async-shell-command "cd ~/t/blogofile.com;/usr/local/bin/blogofile -v build")
  )

;; (defun jm-generate-blogofile ()
;;   "Generate local blogofile"
;;   (interactive)
;;   (org-publish-all)
;;   (start-process-shell-command "blogofile"
;; 							   "*blogofile output*"
;; 							   (format "cd %s;PYTHONPATH=~/workspace/blogofile:~/workspace/blogofile/blogofile python -m blogofile.main -vv build" "~/t/blogofile.com"))
;;   (set-process-sentinel (get-process "blogofile")  #'blogofile-sentinel))
	   
;; (defun blogofile-sentinel (process event)
;;   (when (member (process-status process) '(closed exit))
;; 	(save-excursion
;; 	  (unwind-protect
;; 	    (progn
;; 		  (switch-to-buffer "*blogofile output*")
;; 		  ;; (browse-url "http://localhost:8080")
;; 		  (message "blogofile build"))))))

