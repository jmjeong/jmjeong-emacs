;; git.el 사용 [2009-09-11]
(require 'git)
(require 'git-blame)

(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

(autoload 'magit-status "magit"
  "Magit mode" t)
