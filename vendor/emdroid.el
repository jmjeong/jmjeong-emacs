;;; emdroid.el --- Android Wrappers for Emacs

;; Copyright - (cc) Some Rights Reserved 2007 Jonathan Arkell
;; Author: jonnay <jonnay@jonnay.net>
;; Keywords: java

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
;; Why should eclipse guys get all the fun?  Here are some commands for
;; working with android

;;;  Install:
;; 1)  add to your .emacs somehow.
;; 2)  customize-group emdroid, and set the Emdroid Tools Dir to
;;     the location of the Adroid SDK Tools Directory.
;; 2a) optionally set the name of the emdroid activity creator
;;     executeable

;;; Use:
;; emdroid-emulator
;;   Execute the emulator.
;; emdroid-install
;;   Asks for a file to install, and installs it on the emulator
;; emdroid-dmesg
;;   Prints out the kernel dmesg
;; emdroid-info
;;   Displays info of the device

;;; Bugs:
;; - I think my defcustoms are bunk, and the defaults have embedded quotes.

;;; Plan:
;; - Better usage and install docco
;; - Minor mode with key bindings
;; - more (useful) wrappers around adb commands
;; - wrappers around the ohter commands
;; - integration with jde, especially the debugger
;; - maybe some kinda sql-sqlite integration as well?
;; - dired hooks to copy files to and from the device easily?

;;; History:
;; * 0.1.1 - Added a real emacs prelude
;;         - Added command to start emulator
;;         - Added adb wrapper, it kinda sucks but is at least functional.
;;         - Wrote some wrapper commands
;; * 0.1.0 - First Ever version on EmacsWiki.  

;; Imported from Android.el
(defvar android-jdb-port-history '("8700")
 "history of ports supplied to `android-jdb'")

(defvar android-jdb-project-root-history '()
 "history of project roots supplied to `android-jdb'")

(defvar android-jdb-history nil
 "history of commands supplied to `android-jdb'")

(defvar android-jdb-package-history nil
 "history of android package supplied to various command")

(defvar android-jdb-activity-class-history '()
 "history of activity classes supplied to `start-android-activity'")

(defvar android-adb-direction ""
  "adb command target direction")

(defcustom  android-jdb-command-name "jdb"
  "Name of Java debugger."
  :type 'string
  :group 'android)

(defcustom android-project-root nil
 "This is where your Android project root is stored."
  :type 'directory
 :group 'android )

(defcustom android-apk nil
 "This is where your Android Application Package is stored."
 :type 'string
 :group 'android)

(defcustom android-activity-class nil
 "This is where your Android Activity class is stored."
 :type 'string
 :group 'android)


(defun android-read-project-root ()
 (if (or (string-match "XEmacs" emacs-version)
         (>= emacs-major-version 22))
     (read-file-name "Android project root: "
                     android-project-root
                     nil
                     t
                     nil
                     'file-directory-p)
   (labels ((read-directory ()
                            (read-file-name "Android project root: "
                                            android-project-root
                                            nil
                                            t
                                            nil)))
     (do ((entered-root (read-directory) (read-directory)))
         ((and entered-root
               (file-directory-p entered-root))
          (expand-file-name entered-root))))))


;; Import ends.

(defgroup emdroid nil
  "Customizations for the Emdroid Package."
  :version "0.1"
  :group 'emdroid)

(defcustom emdroid-tools-dir
  "/Users/Joshua/Development/Android/android-sdk-mac_x86-1.5_r3/tools"
  "Directory where the emdroid tools are.  i.e  /android_sdk_windows_m3-rc22a/tools/"
  :group 'emdroid
  :type 'directory)

(defcustom emdroid-android
  "android"
  "android executable sdk manager"
  :group 'emdroid
  :type 'string)

(defvar emdroid-emulator-process nil
  "Variable storing the emulator process state.")

(defvar emdroid-ddms-process nil
  "Variable storing DDMS server process state.")

(defvar emdroid-buffer nil
  "The output buffer for emdroids ADB.")

(defun emdroid-android-cmd (command)
  "Execute android sdk manager with given command parameter"
  (interactive "sandroid ")
  (shell-command (concat emdroid-tools-dir "/" emdroid-android " " command) "*Android Command*"))

(defun emdroid-tool-cmd (command)
  "Execute android SDK tool command"
  (interactive "scommand: ")
  (shell-command (concat emdroid-tools-dir "/" command) "*Android Tool Command*"))

(defun emdroid-emulator-live-p ()
  "Returns whether or not the emulator is live"
  (and emdroid-emulator-process
	   (eq (process-status emdroid-emulator-process)
		   'run)))

(defun emdroid-ddms-live-p ()
  "Returns whether or not the ddms server is live"
  (and emdroid-ddms-process
	   (eq (process-status emdroid-ddms-process)
		   'run)))

(defun emdroid-emulator (avd)
  "Executes the emulator"
  (interactive "sAndroid Virtual Device: ")
  (if (emdroid-emulator-live-p)
	  (message "Emulator already running.")
	  (progn
	   (setq emdroid-emulator-process (start-process "em-droid-emulator" nil (concat emdroid-tools-dir "/emulator") "-avd" avd))
	   (message "Launching emulator."))))

(defun emdroid-adb (command args)
  (if (not (buffer-live-p emdroid-buffer))
	  (setq emdroid-buffer (get-buffer-create "*ADB*")))  
  (pop-to-buffer emdroid-buffer nil)
  (end-of-buffer)
  (shell-command (concat emdroid-tools-dir "/adb " android-adb-direction " " command " " args) 'true))

(defun emdroid-install (apk)
  "Installs an android APK"
  (interactive (list (read-file-name "Android Application Package (.apk): "
                                      android-apk
				      nil
                                      t
                                      nil
                                      nil)))
  (setq android-apk apk)
  (emdroid-adb "install -r" android-apk))


(defun emdroid-dmesg ()
  "Show the linux dmesg"
  (interactive)
  (emdroid-adb "shell" "dmesg"))

(defun emdroid-info ()
  "Show info about the device.  For now just the serial and device #"
  (interactive)
  (emdroid-adb "get-product" "")
  (emdroid-adb "get-serialno" ""))

;; Added by Joshua on 2009-03-30 Mon PM  2:17
(defun emdroid-uninstall (package)
  "Uninstalls an android APK"
  (interactive "fPackage To Uninstall: ")
  (emdroid-adb "uninstall" package))

(defun emdroid-run-activity (package class)
 "Start the activity PACKAGE/CLASS in the Android emulator."
 (interactive
  (list
   (read-from-minibuffer "Package(without activity name): "
			 (car android-jdb-package-history)
			 nil
			 t
			 'android-jdb-package-history)
   (read-from-minibuffer "Activity Java class: "
         (car android-jdb-activity-class-history)
         nil
         t
         'android-jdb-activity-class-history)))
 (emdroid-adb "shell" (format "am start -n %s/%s.%s" package package class)))


(defun emdroid-debug-activity (package class)
  "Start the activity PACKAGE/CLASS within the debugger in the Android emulator."
  (interactive
   (list
    (read-from-minibuffer "Package: "
			  (car android-jdb-package-history)
			  nil
			  t
			  'android-jdb-package-history)
    (read-from-minibuffer "Activity Java class: "
			  (car android-jdb-activity-class-history)
			  nil
			  t
			  'android-jdb-activity-class-history)))
  (emdroid-adb "shell" (format "am start -D -n %s/%s.%s" package package class))
  (emdroid-ddms)
  (call-interactively 'emdroid-jdb))

(defun emdroid-set-adb-target (direction)
  "Set adb command direction between device & emulator."
  (interactive "sADB command direction ( 'd' for device / 'e' for emulator ) : ")
  (if (or (string-equal "d" direction)
	  (string-equal "e" direction))
      (setq android-adb-direction (format "-%s" direction))
    (setq android-adb-direction " "))
  (message android-adb-direction)
)

(defun emdroid-jdb (port root)
 "Set GUD+JDB up to run against Android on PORT in directory ROOT."
 (interactive
  (list
   (read-from-minibuffer "Activity's JDWP DDMS port: "
                     (car android-jdb-port-history)
                     nil
                     t
                     'android-jdb-port-history)
                    (android-read-project-root)))
 (setq android-project-root root)
 (let ((jdb-command
        (format "%s -attach localhost:%s -sourcepath%s"
                android-jdb-command-name
                port
                (format "%ssrc/" root))))
    (message "Debug command:%s" jdb-command )
   (if (not (string= jdb-command (car android-jdb-history)))
       (push jdb-command android-jdb-history))
   (jdb jdb-command)))

(defun emdroid-ddms()
  "Executes ddms"
  (interactive)
  (if (emdroid-ddms-live-p)
      (message "DDMS server is already running.")
    (progn
      (setq emdroid-ddms-process (start-process "em-driod-ddms" nil (concat emdroid-tools-dir "/ddms")))
      (message "Launching DDMS server."))))

(provide 'emdroid)
;;; emdroid.el ends here
