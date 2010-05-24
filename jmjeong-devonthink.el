;; Jaemok Jeong(jmjeong@gmail.com)
;;
;; [2010-05-20 Thu]
;;
;; Export org-file into html file and save it into DEVONthink Pro

(defun org-export-to-devonthink (arg)
  "Call `org-export-to-devonthink` with output to a temporary buffer.
  No file is created."
  (interactive "P")
  (let (content)
	(org-export-as-html arg nil nil "*Org HTML Export*")
	(switch-to-buffer "*Org HTML Export*")
	(setq content (buffer-string))
	(kill-buffer "*Org HTML Export*")
	(setq content (replace-regexp-in-string (regexp-quote "\"") "\\\"" content t t))
	(do-applescript
	 (format "tell application id \"com.devon-technologies.thinkpro2\"
              create record with {name:\"%s\", type:html, plain text:\"%s\"} in current group
              end tell
             " (buffer-name) content))
	)
  )

(global-set-key [f8] 'org-export-to-devonthink)
