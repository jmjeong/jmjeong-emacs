;; git.el 사용 [2009-09-11]
(require 'git)
(require 'git-blame)

(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

(autoload 'magit-status "magit" "Magit mode" t)
(require 'magit-svn)


; mo-git-blame

(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)