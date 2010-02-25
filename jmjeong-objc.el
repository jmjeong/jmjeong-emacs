;; obj-c-modennp
(require 'objc-c-mode)
(setq auto-mode-alist (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mm$" . objc-mode) auto-mode-alist))

(defvar xcode-compile-sdks nil)
(defvar xcode-compile-sdk-history nil)

(dolist (line
         (split-string
          (with-temp-buffer
            (call-process "xcodebuild" nil t nil "-showsdks")
            (buffer-string))
          "\n" t)
         )
  (let ((comps (split-string line "-sdk " t)))
    (if (> (length comps) 1)
        (add-to-list 'xcode-compile-sdks (car (last comps)))
      )
    )
  )

(defun xcode-compile ()
  (interactive)
  (let ((command "xcodebuild -activeconfiguration -activetarget"))
    (setq command
          (concat
           command
           (if xcode-compile-sdks
               (let ((default-sdk (or (car xcode-compile-sdk-history) (car xcode-compile-sdks))))
                 (concat
                  " -sdk "
                  (completing-read
                   (format "Compile with sdk (default %s): " default-sdk)
                   xcode-compile-sdks
                   nil
                   t
                   nil
                   'xcode-compile-sdk-history
                   default-sdk
                   )
                  )
                 )
             )
           (let ((dir ".") (files nil))
             (while
                 (progn
                   (setq files (directory-files dir nil "\\.xcodeproj\\'"))
                   (and (not (string-equal "/" (expand-file-name dir))) (null files))
                   )
               (setq dir (concat (file-name-as-directory dir) ".."))

               )
             (unless (null files) (concat " -project " (file-name-as-directory dir) (car files)))
             )
           )
          )
    (compile (read-string "Compile command: " (concat command " ")))
    )
  )

(define-key global-map [f7] 'xcode-compile)

;;
;; [2010-02-11 Thu]
;; xcodeproj가 있는 directory에 대해서는 .h를 objc-mode로 동작하도록 함
;;

(defun xcode-root ()
  "Look for .xcodeproj file to find project root of xcode application."
  (let ((cwd default-directory)
        (found nil)
        (max 10))
    (while (and (not found) (> max 0))
      (if (directory-files cwd nil "\\.xcodeproj\\'") 
        (setq found cwd)
        (setq cwd (concat cwd "../") max (- max 1))))
    (and found (expand-file-name found))))

;; 오동작..  있는 경우에는 추가하고 없는 경우에는 삭제 하도록 하거나 아니면 priority를 지정하는 방안을 고민해 봐야
;;
(add-hook 'dired-mode-hook (lambda () (when (xcode-root) (setq auto-mode-alist (cons '("\\.h$" . objc-mode) auto-mode-alist)))))
(add-hook 'find-file-hooks (lambda () (when (xcode-root) (setq auto-mode-alist (cons '("\\.h$" . objc-mode) auto-mode-alist)))))
