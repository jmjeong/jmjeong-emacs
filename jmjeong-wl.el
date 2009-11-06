;; mode:-*-emacs-lisp-*-
;; wanderlust 
(setq 
  elmo-maildir-folder-path "~/IMAP"          ;; where i store my mail

  wl-stay-folder-window t                       ;; show the folder pane (left)
  wl-folder-window-width 25                     ;; toggle on/off with 'i'
  
  wl-smtp-posting-server "localhost"            ;; put the smtp server here
  wl-local-domain "nemustech.com"          ;; put something here...
  wl-message-id-domain "nemustech.com"     ;; ...

  wl-from "Jaemok Jeong <jmjeong@nemustech.com>"                  ;; my From:

  ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path 
  ;; the '.'-prefix is for marking them as maildirs
  wl-fcc ".sent"                       ;; sent msgs go to the "sent"-folder
  wl-fcc-force-as-read t               ;; mark sent messages as read 
  wl-default-folder ".inbox"           ;; my main inbox 
  wl-draft-folder ".company"            ;; store drafts in 'postponed'
  wl-trash-folder ".trash"             ;; put trash in 'trash'
  wl-spam-folder ".trash"              ;; ...spam as well
  wl-queue-folder ".queue"             ;; we don't use this

  ;; check this folder periodically, and update modeline
  wl-biff-check-folder-list '(".todo") ;; check every 180 seconds
                                       ;; (default: wl-biff-check-interval)

  ;; hide many fields from message buffers
  wl-message-ignored-field-list '("^.*:")
  wl-message-visible-field-list
  '("^\\(To\\|Cc\\):"

    "^Subject:"
    "^\\(From\\|Reply-To\\):"
    "^Organization:"
    "^Message-Id:"
    "^\\(Posted\\|Date\\):"
    )
  wl-message-sort-field-list
  '("^From"

    "^Organization:"
    "^X-Attribution:"
     "^Subject"
     "^Date"
     "^To"
     "^Cc"))

