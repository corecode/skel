;; list of packages to be installed by the starter kit.
;; (setq starter-kit-packages (list 'kill-ring-search
;;                                  'jump
;;                                  'inflections
;;                                  'findr
;;                                  'c-eldoc
;;                                  'css-mode
;;                                  'guess-style
;;                                  'htmlize
;;                                  'http-post-simple
;;                                  'auto-dictionary
;;                                  'Save-visited-files
;;                                  'smart-operator
;;                                  'find-file-in-project
;;                                  'gist
;;                                  'idle-highlight
;;                                  'inf-ruby
;;                                  'project-local-variables
;;                                  ))

(setq-default starter-kit-packages
              '(
                kill-ring-search
                lisppaste
                xml-rpc
                org
                paredit
                jump
                inflections
                findr
                ruby-compilation
                c-eldoc
                css-mode
                guess-style
                highlight-symbol
                htmlize
                http-post-simple
                auto-dictionary
                Save-visited-files
                ;; slime
                smart-operator
                smex
                textmate
                ;; yasnippet-bundle
                ;; auctex
                find-file-in-project
                gist
                idle-highlight
                inf-ruby
                magit
                project-local-variables
                ruby-mode
                yaml-mode
                yari))

;; to actually install, eval
;; (starter-kit-elpa-install)

(setq-default el-get-recipe-path (list "~/.emacs.d/vendor/el-get/recipes/"))
(setq-default el-get-sources
              '(
                ac-slime
                auctex
                ;; auto-complete
                browse-kill-ring
                csharp-mode
		color-theme
                ecb
                emacs-goodies-el
                json
                js2-mode
                ;; emacs-textmate
                ;; flymake-point
                ;; flymake-ruby
                ;; grep+
                ;; highlight-parentheses
                ;; highlight-symbol
                ;; hl-sexp
                ;; magit
                ;; markdown-mode
                multi-term
                org-mode
                ;; package
                ;; paredit
                pastebin
                php-mode
                popup-kill-ring
                ;; python-mode
                rinari
                ;; ruby-compilation
                ruby-block
                ruby-end
                ruby-mode
                rvm
                ;; session
                sinasi
                ;; slime
                ;; smart-tab
                ;; smex
                ;; sudo-save
                xcscope
                yari
                ;; yaml-mode
                yasnippet
                ))

;; install directly
(require 'el-get)
(condition-case err
    (el-get 'sync)
  (error
   (message "Problems installing el-get packages: %s" (error-message-string err))))
