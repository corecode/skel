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

(setq starter-kit-packages (list 'kill-ring-search
                                 'lisppaste
                                 'xml-rpc
                                 'org
                                 'paredit
                                 'jump
                                 'inflections
                                 'findr
                                 'ruby-compilation
                                 'c-eldoc
                                 'css-mode
                                 'guess-style
                                 'highlight-symbol
                                 'htmlize
                                 'http-post-simple
                                 'auto-dictionary
                                 'Save-visited-files
                                 ;; 'slime
                                 'smart-operator
                                 'smex
                                 'textmate
                                 'yasnippet-bundle
                                 'auctex
                                 'find-file-in-project
                                 'gist
                                 'idle-highlight
                                 'inf-ruby
                                 'magit
                                 'project-local-variables
                                 'ruby-mode
                                 'yaml-mode
                                 'yari))

;; to actually install, eval
;; (starter-kit-elpa-install)

(setq el-get-recipe-path (list "~/.emacs.d/vendor/el-get/recipes/"))
(setq el-get-sources
      '(
        ac-slime
        ;; auctex
        ;; auto-complete
        browse-kill-ring
        ;; emacs-goodies-el
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
        ;; org-mode
        ;; package
        ;; paredit
        pastebin
        popup-kill-ring
        ;; python-mode
        rinari
        ;; ruby-compilation
        ;; ruby-block
        ;; ruby-end
        ;; ruby-mode
        ;; rvm
        ;; session
        ;; slime
        ;; smart-tab
        ;; smex
        ;; sudo-save
        ;; xcscope
        yari
        ;; yaml-mode
        ;; yasnippet
        ))

;; install directly
(require 'el-get)
(el-get 'wait)
