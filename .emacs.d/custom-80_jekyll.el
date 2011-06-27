(require 'jekyll)

(set-default 'jekyll-directory "~/Documents/hp/")
(set-default 'jekyll-post-template
             "---\nlayout: post\ntitle: %s\n---\n\n")

(global-set-key (kbd "C-c b n") 'jekyll-draft-post)
(global-set-key (kbd "C-c b P") 'jekyll-publish-post)
(global-set-key (kbd "C-c b p") (lambda () 
                                  (interactive)
                                  (find-file
                                   (format "%s/_posts/" jekyll-directory))))
(global-set-key (kbd "C-c b d") (lambda () 
                                  (interactive)
                                  (find-file
                                   (format "%s/_drafts/" jekyll-directory))))
