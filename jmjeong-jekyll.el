(require 'markdown-mode)
(require 'jekyll)
(jekyll-init-keybindings)
(setq jekyll-directory "/Users/jmjeong/git/jmjeong.com/")
(setq jekyll-post-template "---\nlayout: post\ntitle: %s\nmodified: \ncategory: \ntags: \nimage:\n  teaser:\n---\n")
(setq jekyll-post-ext ".md")