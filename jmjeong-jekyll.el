(require 'markdown-mode)
(require 'jekyll)
(jekyll-init-keybindings)
(setq jekyll-directory "/Users/jmjeong/Dropbox/Documents/jmjeong.com/")
(setq jekyll-post-template "---\nlayout: post\ntitle: %s\ndescription: \ncategory: \ntags: \n---\n{%% include JB/setup %%}\n\n")
(setq jekyll-post-ext ".md")