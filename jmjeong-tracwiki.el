;;; Trac-wiki Setting

(autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t)
(setq trac-projects
	  '(("incubator"
         :endpoint "http://podo.jmjeong.com/projects/jupiter/login/xmlrpc")
		("umon"
         :endpoint "http://podo.jmjeong.com/trac/umon/login/xmlrpc")
		("iphone"
         :endpoint "http://podo.jmjeong.com/projects/iphone/login/xmlrpc")
		("intern"
         :endpoint "http://podo.jmjeong.com/trac/InternIphone/login/xmlrpc")
		)	  
	  )
(global-set-key (kbd "C-= C-`") 'trac-wiki)
