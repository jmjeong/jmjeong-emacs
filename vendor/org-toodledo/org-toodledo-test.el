(require 'org-toodledo)

(defun org-toodledo-test-message (str &rest args)
  (save-excursion
    (set-buffer org-toodledo-test-msg-buf)
    (end-of-buffer)
    (insert (concat (apply 'format (append (list str) args)) "\n"))))

(defun org-toodledo-test-check (value str &rest args)
  (setq org-toodledo-test-count (1+ org-toodledo-test-count))
  (cond 
   ((not (null value))
    (setq org-toodledo-test-pass (1+ org-toodledo-test-pass))
    (org-toodledo-test-message "PASSED: %s" (apply 'format (append (list str) args))))
   
   (t 
    (setq org-toodledo-test-fail (1+ org-toodledo-test-fail))
    (org-toodledo-test-message "FAILED: %s" (apply 'format (append (list str) args))))))

(defun org-toodledo-test-equal (value1 value2 str &rest args)
  (setq org-toodledo-test-count (1+ org-toodledo-test-count))
  (cond 
   ((equal value1 value2)
    (setq org-toodledo-test-pass (1+ org-toodledo-test-pass))
    (org-toodledo-test-message "PASSED: %s" (apply 'format (append (list str) args))))
   
   (t
    (setq org-toodledo-test-fail (1+ org-toodledo-test-fail))
    (org-toodledo-test-message "FAILED: %s [%S != %S]" 
             (apply 'format (append (list str) args))
             value1 value2 
             ))))

(defun org-toodledo-test(&rest tests)
  "Org-toodledo tests"
  (interactive)
 
  (let ((org-toodledo-test-count 0)
        (org-toodledo-test-pass 0)
        (org-toodledo-test-fail 0)
        (debug-on-error t)
        (org-toodledo-test-msg-buf (get-buffer-create "*Org-toodledo-test-log*"))
        )
    (setq org-toodledo-test-mode t)
    (pop-to-buffer org-toodledo-test-msg-buf)
    (erase-buffer)
    
    (org-toodledo-test-message "Starting tests")

    (let ((buf1 (get-buffer-create "*Org-toodledo-test-1*"))
          (buf2 (get-buffer-create "*Org-toodledo-test-2*")))

      (when (or (member 'basic tests) (null tests))
        ;; Create buf1 in org-mode, fill with a few tasks
        (org-toodledo-test-message "TEST: Creating 3 tasks")
        (org-toodledo-test-setup-buffer buf1)
        (org-toodledo-test-create-tasks 3)
        (org-toodledo-initialize "TASKS")
        
        ;; Create buf2 in org-mode, initialize, pull in tasks
        (org-toodledo-test-message "TEST: Syncing 3 tasks")
        (org-toodledo-test-setup-buffer buf2)
        (org-toodledo-initialize "TASKS")
        (org-toodledo-test-verify-tasks buf2 "Task 1" "Task 2" "Task 3")
        
        ;; Modify Task 1 and sync
        (org-toodledo-test-message "TEST: Modifying Task 1")
        (org-toodledo-test-goto-task "Task 1")
        (end-of-line)
        (insert-string " - modified")
        (org-toodledo-test-equal (org-toodledo-sync) '(1 0 0 0 1 0) "Synced out 1 modified task")
        
        ;; Back to buf1, sync -- verify
        (org-toodledo-test-message "TEST: Syncing modified Task 1")
        (set-buffer buf1)
        (org-toodledo-test-equal (org-toodledo-sync) '(1 1 0 0 0 0) "Synced in 1 modified task")
        (org-toodledo-test-verify-tasks buf1 "Task 1 - modified" "Task 2" "Task 3")
        
        ;; Modify Task 3 and sync
        (org-toodledo-test-message "TEST: Modifying Task 3")
        (org-toodledo-test-goto-task "Task 3")
        (end-of-line)
        (insert-string " - modified")
        (org-toodledo-test-equal (org-toodledo-sync) '(1 0 0 0 1 0) "Synced out 1 modified task")

        ;; Back to buf2, sync -- verify
        (org-toodledo-test-message "TEST: Syncing modified Task 3")
        (set-buffer buf2)
        (org-toodledo-test-equal (org-toodledo-sync) '(1 1 0 0 0 0) "Synced in 1 modified task")
        (org-toodledo-test-verify-tasks buf2 "Task 1 - modified" "Task 2" "Task 3 - modified")

        ;; Compare all tasks between both buffers
        (org-toodledo-test-message "TEST: Comparing all tasks between buffers")
        (org-toodledo-test-compare-tasks buf1 buf2 "Task 1 - modified" "Task 2" "Task 3 - modified")

        (org-toodledo-test-message "TEST: Cleanup")
        (org-toodledo-test-cleanup)
        )
      
      ;; 
      ;; Encoding special chars
      ;;
      (when (or (member 'special tests) (null tests))
        (org-toodledo-test-message "TEST: Encoding special characters")
        (org-toodledo-test-setup-buffer buf2)
        (org-toodledo-test-message "Initializing buf2: %S" (org-toodledo-initialize "TASKS"))

        (org-toodledo-test-setup-buffer buf1)
        (org-toodledo-test-message "Initializing buf1: %S" (org-toodledo-initialize "TASKS"))

        (goto-char (point-max))
        (insert-string "** TODO ORGTOODLEDOTEST:Task é字\nBody é字")
        (org-toodledo-test-equal (org-toodledo-sync) '(1 0 0 1 0 0) "Synced out 1 new task with special chars")

        (org-toodledo-test-setup-buffer buf2)
        (org-toodledo-test-equal (org-toodledo-sync) '(1 1 0 0 0 0) "Synced in 1 task with special chars")
        (org-toodledo-test-compare-tasks buf1 buf2 "Task é字")

        (org-toodledo-test-message "TEST: Cleanup")
        (org-toodledo-test-cleanup)
        )
      
      ;;
      ;; Bulk test -- make sure more than 50 works
      ;;
      (when (or (member 'bulk tests) (null tests))
        (org-toodledo-test-setup-buffer buf2)
        (org-toodledo-initialize "TASKS")

        (org-toodledo-test-setup-buffer buf1)
        (org-toodledo-initialize "TASKS")

        (org-toodledo-test-message "TEST: Create 60 tasks")
        (set-buffer buf1)
        (org-toodledo-test-create-tasks 60 2 100)
        (org-toodledo-test-equal (org-toodledo-sync) '(60 0 0 60 0 0) "Synced 60 new tasks")
        
        (org-toodledo-test-setup-buffer buf2)
        (org-toodledo-test-equal (org-toodledo-sync) '(60 60 0 0 0 0) "Synced in 60 tasks")

        (org-toodledo-test-message "TEST: Cleanup")
        (org-toodledo-test-cleanup)
        )

      ;; 
      ;; Folder tests
      ;;
      (when (or (member 'folder tests) (null tests))
        (org-toodledo-test-setup-buffer buf2)
        (org-toodledo-initialize "TASKS")

        (org-toodledo-test-setup-buffer buf1)
        (org-toodledo-initialize "TASKS")

        (org-toodledo-test-message "TEST: Create 1 task with a folder")
        (set-buffer buf1)
        (org-toodledo-test-create-tasks 1 2 200)
        (org-toodledo-test-goto-task "Task 200")
        (let ((task (org-toodledo-parse-current-task)))
          (aput 'task "folder" (org-toodledo-folder-to-id "TESTFOLDER"))
          (org-toodledo-insert-new-task task t t))
        (org-toodledo-test-equal (org-toodledo-sync) '(1 0 0 1 0 0) "Synced 1 new task")
        
        (org-toodledo-test-setup-buffer buf2)
        (org-toodledo-test-equal (org-toodledo-sync) '(1 1 0 0 0 0) "Synced in 1 tasks")

        (org-toodledo-test-message "TEST: Cleanup")
        (org-toodledo-test-cleanup)
        )
      

      ;; All done
      (setq org-toodledo-test-mode nil)
      
      (org-toodledo-test-message "Tests complete: %d/%d tests passed" 
               (- org-toodledo-test-count org-toodledo-test-fail) org-toodledo-test-count)
      )
    )
  )

(defun org-toodledo-test-goto-task (title &optional buffer)
  (if buffer
      (set-buffer buffer)
    (setq buffer (current-buffer)))
  (goto-char (point-min))
  (org-toodledo-test-check (re-search-forward (concat "ORGTOODLEDOTEST:" title "\\b") nil t)
                           "Find task '%s' in buffer '%S'" title (current-buffer))
  )

(defun org-toodledo-test-setup-buffer (name)
  (let ((buf (get-buffer-create name)))
    (set-buffer buf)
    (erase-buffer)
    (when (not (eq major-mode 'org-mode))
      (org-mode))
    (insert-string "* TASKS\n")
    )
  )

(defun org-toodledo-test-create-tasks (num &optional level start)
  (let (result)
    (do ((i 0 (1+ i))) ((>= i num) nil)
      (insert-string (format "%s TODO ORGTOODLEDOTEST:Task %d\n" (make-string (or level 2) ?*) (+ i (or start 1))))
      (setq result (append result (list (format "Task %d" i)))))
    result))


(defun org-toodledo-test-cleanup()
  "Delete all test tasks"
  (interactive)
  (condition-case nil
      (progn
        (setq org-toodledo-test-mode t)
        (let ((buf (get-buffer-create "*Org-toodledo-cleanup*")))
          (set-buffer buf)
          (erase-buffer)
          (when (not (eq major-mode 'org-mode))
            (org-mode))
          (insert-string "* TASKS\n")
          (org-toodledo-initialize "TASKS")
          
          ;; Delete all tasks
          (goto-char (point-min))
          (while (re-search-forward "ORGTOODLEDOTEST" nil t)
            (org-toodledo-mark-task-deleted))
          
          (org-toodledo-sync)
          )
        )
    )
  )

(defun org-toodledo-test-compare-tasks (buf1 buf2 &rest titles)
  (mapcar 
   (lambda (title)
     (let (task1 task2)
       ;; Get the task from buf1
       (set-buffer buf1)
       (when (org-toodledo-test-goto-task title)
         (setq task1 (org-toodledo-parse-current-task))
         
         ;; Get the task from buf2
         (set-buffer buf2)
         (when (org-toodledo-test-goto-task title)
           (setq task2 (org-toodledo-parse-current-task))
           
           ;; Compare all fields
           (mapc 
            (lambda (task1-assoc)
              (let* ((key1 (car task1-assoc))
                     (value1 (or (cdr task1-assoc) "0"))
                     (task2-assoc (assoc key1 task2))
                     (value2 (or (cdr task2-assoc) "0")))
                (when (member key1 '("id" "title" "status" "completed" "repeat" "repeatfrom" "context" "duedate" 
                                     "startdate" "folder" "goal" "priority" "note" "length" "parent" "hash"))
                  (org-toodledo-test-message "Comparing key %s='%s'" key1 value1)
                  (org-toodledo-test-check task2-assoc "Found key '%s' in task2" key1)
                  (org-toodledo-test-equal value1 value2 "Key '%s' values match" key1))))
            task1)))))
   titles
   )
  )

(defun org-toodledo-test-verify-tasks (buffer &rest titles)
  (set-buffer buffer)
  (goto-char (point-min))
  (let ((count 0))
    (while (re-search-forward "ORGTOODLEDOTEST" nil t)
      (setq count (1+ count)))
    (org-toodledo-test-equal 
     count (length titles) "Verify buffer %S has %d tasks" buffer (length titles))
    )
  
  (mapcar 
   (lambda (title)
     (org-toodledo-test-check 
      (save-excursion 
        (goto-char (point-min))
        (re-search-forward (concat "ORGTOODLEDOTEST:" title "\\b") nil t))
      "Find task in buffer %S: %s" buffer title))
   titles))

(provide 'org-toodledo-test)
