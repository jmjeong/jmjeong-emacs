(require 'org2blog-autoloads)

(setq org2blog-server-url "http://jmjeong.com/xmlrpc.php"
	  org2blog-server-user "jmjeong"
	  org2blog-server-weblog-id "")
(setq org2blog-use-tags-as-categories nil)


(setq org2blog-blog-alist
       '(("jmjeong"
          :url "http://jmjeong.com/xmlrpc.php"
          :username "jmjeong"   
          :default-title "Org"
          :default-categories ("org2blog" "emacs")
          :tags-as-categories nil)
		 ))