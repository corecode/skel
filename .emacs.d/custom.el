;; below automatically added settings by custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
 '(auto-indent-next-pair-timer-interval (quote ((default 0.0005))))
 '(custom-safe-themes
   (quote
    ("f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default)))
 '(doc-view-resolution 200)
 '(erc-modules
   (quote
    (completion move-to-prompt stamp spelling truncate hl-nicks netsplit fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt menu list scroll-conservatively)))
 '(godoc-command "godoc")
 '(gofmt-command "goimports")
 '(jabber-account-list (quote (("2@0x2c.org"))))
 '(jabber-otr-message-history t)
 '(nrepl-popup-stacktraces t)
 '(nrepl-popup-stacktraces-in-repl t)
 '(org-agenda-files (quote ("~/.org/notes.org" "~/.org/todo.org")))
 '(org-journal-file-format "%Y%m")
 '(org-log-into-drawer t)
 '(org-time-clocksum-format
   (quote
    (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(package-selected-packages
   (quote
    (systemtap-mode org org-clock-csv git-commit-mode zoom-frm yari yaml-mode wgrep w3m undo-tree top-mode tide tern-auto-complete smooth-scrolling smex smart-tab smart-forward slime shader-mode session scad-preview rvm ruby-end ruby-compilation ruby-block request rainbow-delimiters quack qml-mode python-mode powerline pkgbuild-mode paredit org-journal nrepl nodejs-repl nim-mode nhexl-mode multifiles multi-term move-text minitest mediawiki markdown-mode mark-multiple magit-svn magit-push-remote magit-gh-pulls lua-mode key-chord jump-char js2-refactor jabber-otr ipython iedit ido-ubiquitous highlight-indentation helm-gist haskell-mode graphviz-dot-mode goto-chg go-stacktracer go-scratch go-rename go-errcheck go-eldoc go-dlv go-autocomplete glsl-mode gitignore-mode gitconfig-mode forth-mode fold-this flymake-ruby flymake-jshint flymake-go flymake-cursor flycheck-typescript-tslint find-file-in-project fill-column-indicator ess erc-hl-nicks eproject elisp-slime-nav edit-server dired-details diminish csharp-mode confluence company color-moccur cmake-project cmake-mode change-inner browse-kill-ring auto-indent-mode auctex android-mode ace-jump-mode ac-nrepl)))
 '(safe-local-variable-values
   (quote
    ((org-latex-minted-options
      ("fontsize" "\\tiny")
      ("linenos" ""))
     (org-latex-minted-options
      ("fontsize" "\\footnotesize")
      ("linenos" ""))
     (org-latex-minted-options
      ("fontsize" "\\scriptsize")
      ("linenos" ""))
     (org-export-latex-minted-options
      ("fontsize" "\\scriptsize")
      ("linenos" ""))
     (org-latex-listings . minted)
     (org-export-latex-listings . minted)
     (org-export-latex-listings quote minted)
     (TeX-master . t)
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode))
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t)
     (ada-prj-default-comp-opt . "-gnatq -gnatQ -gnat2012")
     (noweb-default-code-mode . R-mode)
     (whitespace-line-column . 100)
     (c-default-style . gnu)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby")
     (lexical-binding . t))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "0x2c.org")
 '(smtpmail-smtp-service 587)
 '(smtpmail-smtp-user "2")
 '(smtpmail-stream-type (quote starttls))
 '(verilog-auto-endcomments nil)
 '(verilog-auto-lineup (quote all))
 '(verilog-auto-newline nil)
 '(verilog-highlight-grouping-keywords t)
 '(verilog-indent-begin-after-if nil)
 '(verilog-indent-level-module 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
