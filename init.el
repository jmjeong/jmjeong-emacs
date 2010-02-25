;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jaemok Jeong's Emacs Environment
;;   Jaemok Jeong <jmjeong@gmail.com>
;;   http://jmjeong.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom elisp-root-dir "~/my-dot-emacs"
  "The root directory where elisp is installed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library Path 
;;
;; Note: All emacs configraiton is underneath 'elisp-root-dir'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path elisp-root-dir)
(add-to-list 'load-path (concat elisp-root-dir "/auto-install"))
(add-to-list 'load-path (concat elisp-root-dir "/vendor"))
(add-to-list 'load-path (concat elisp-root-dir "/vendor/bbdb"))
(add-to-list 'load-path (concat elisp-root-dir "/vendor/yasnippet"))
(add-to-list 'load-path (concat elisp-root-dir "/vendor/emacs-w3m"))
(add-to-list 'load-path (concat elisp-root-dir "/vendor/pylookup"))

;; Add all top-level subdirectories of my-dot-emacs
;; (progn (cd "~/my-dot-emacs")
;; 	   (load "subdirs"))

;; add-to-list emacs lisp package archive directory
(let ((default-directory (concat elisp-root-dir "/vendor/elpa")))
  (load "subdirs"))

(require 'emacs-type)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 자주 쓰이는 파일에 대한 접근 Key정의
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "jmjeong-shortcut")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; password : real-passwd는 다른곳에 보관
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "jmjeong-passwd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 한글 세팅
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "jmjeong-hangul")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "jmjeong-system")			; system depedent setting
(load-library "jmjeong-func")			; jmjeong's local function def
(load-library "jmjeong-key")			; local key binding
(load-library "jmjeong-filecache") 		; file cache
(load-library "jmjeong-elpa")			; emacs lisp package archive
;(load-library "jmjeong-cedet")			; cedet
;(load-library "jmjeong-icicles")		; icicles
(load-library "jmjeong-visual")			; visual apperance
(load-library "jmjeong-icomplete") 		; icomplte, buffer setting
;(load-library "jmjeong-ido")			; ido mode
(load-library "jmjeong-doxymacs")		; doxymacs
(load-library "jmjeong-xcscope")		; xcscope
(load-library "jmjeong-cprog")			; c programming
(load-library "jmjeong-calendar")		; lunar calendar and diary stuff
;;(load-library "jmjeong-objc")			; objective-cc
(load-library "jmjeong-python")			; python
(load-library "jmjeong-git")			; git
(load-library "jmjeong-maxframe")		; maxframe
(load-library "jmjeong-autocomplete")	; auto-complete
(load-library "jmjeong-w3m")			; w3m
(load-library "jmjeong-bbdb")			; bbdb
(load-library "jmjeong-php")			; php
(load-library "jmjeong-gnus")			; gnus
(load-library "jmjeong-org")			; org-mode
;(load-library "jmjeong-howm")			; howm
(load-library "jmjeong-tempbuf")		; tempbuf
(load-library "jmjeong-redo")			; redo
(load-library "jmjeong-psvn")			; psvn
(load-library "jmjeong-tuareg")			; tuareg
(load-library "jmjeong-yasnippet")		; yasnippet
(load-library "jmjeong-markdown")		; markdown-mode
(load-library "jmjeong-android")		; android
(load-library "jmjeong-dired")			; dired
(load-library "jmjeong-twitter")		; twitter
(load-library "jmjeong-recentf")		; recentf
(load-library "jmjeong-gtags")			; gtags
(load-library "jmjeong-winmove")		; winmove
(load-library "jmjeong-anything")		; anything
(load-library "jmjeong-gist")			; gist
(load-library "jmjeong-dictionary")		; dictionary
(load-library "jmjeong-autoinstall")	; auto-install

(garbage-collect)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(canlock-password "154f9f29ee266ade1cca710c233702480de625f0")
 '(display-time-mode t)
 '(git-baseline-commit (quote (("/Users/jmjeong/t/blogofile.com/" . "blogofile.com"))) t)
 '(mm-charset-override-alist (quote ((gb2312 . gbk) (iso-8859-1 . windows-1252) (iso-8859-8 . windows-1255) (iso-8859-9 . windows-1254) (x-windows-949 . euc-kr) (unicode-1-1-utf-7 . utf-8))))
 '(org-agenda-files (quote ("/Users/jmjeong/workspace/journal/org/100104~100108.org" "/Users/jmjeong/workspace/journal/org/gitcheetsheet.org" "/Users/jmjeong/workspace/journal/org/index.org" "/Users/jmjeong/workspace/journal/org/iphone-tutorial.org" "/Users/jmjeong/workspace/journal/org/orgmode.org" "/Users/jmjeong/workspace/journal/org/todo.org" "/Users/jmjeong/workspace/journal/org/company/android.org" "/Users/jmjeong/workspace/journal/org/company/interview.org" "/Users/jmjeong/workspace/journal/org/company/kth-MusicAurora.org" "/Users/jmjeong/workspace/journal/org/company/lge.org" "/Users/jmjeong/workspace/journal/org/company/newemployee.org" "/Users/jmjeong/workspace/journal/org/company/sonata.org" "/Users/jmjeong/workspace/journal/org/company/teammeeting.org" "/Users/jmjeong/workspace/journal/org/company/tiffany.org" "/Users/jmjeong/workspace/journal/org/personal/apt09.org" "/Users/jmjeong/workspace/journal/org/personal/buypc.org" "/Users/jmjeong/workspace/journal/org/personal/family-event.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file (concat org-agenda-directory "/notes.org"))
 '(org-emphasis-regexp-components (quote (" 	('\"{" "- 	.,:!?;'\")}[:multibyte:]" " 	
,\"'" "." 1)))
 '(org-export-html-style-include-scripts nil)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-mobile-directory "/Volumes/jmjeong/org")
 '(org-mobile-files (quote (org-agenda-files org-agenda-text-search-extra-files)))
 '(org-mobile-inbox-for-pull "~/workspace/journal/org/from-org.org")
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (\` ((110 "* %u %?" (\, (concat org-agenda-directory "notes.org")) "Notes") (116 "* TODO %?
  %u" (\, (concat org-agenda-directory "todo.org")) "Tasks"))))
 '(org-reverse-note-order t)
 '(safe-local-variable-values (quote ((dired-omit-extensions ".html") (dired-omit-mode . t) (dired-actual-switches . "-lat"))))
 '(scroll-bar-mode nil)
 '(semantic-java-dependency-system-include-path (quote ("/Users/jmjeong/android/1.6_r1.4src/framework/base/core/java/")))
 '(show-paren-mode t)
 '(w3m-use-tab nil))

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diary ((t (:foreground "darkred")))))
