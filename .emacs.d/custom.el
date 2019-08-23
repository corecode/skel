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
 '(org-structure-template-alist
   (quote
    (("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse"))))
 '(org-time-clocksum-format
   (quote
    (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
 '(package-selected-packages
   (quote
    (magit multiple-cursors expand-region counsel swiper ivy undo-tree fzf rg try rgrep visual-regexp zoom-frm znc yasnippet-snippets yari yaml-mode xml-rpc which-key wgrep w3m use-package top-mode tide term-projectile smooth-scrolling smex smart-forward slime session rvm ruby-end ruby-compilation ruby-block rainbow-mode rainbow-delimiters racer quack python-mode pydoc-info pydoc powerline pkgbuild-mode paredit org-clock-csv org-bullets number multifiles multi-term move-text markdown-mode mark-multiple magit-svn magit-popup magit-gh-pulls key-chord jump-char js2-refactor jedi jabber iedit ido-completing-read+ highlight haskell-mode graphviz-dot-mode graphql goto-chg go-mode go gitignore-mode gitconfig-mode gist ghub forth-mode fold-this flymake-ruby flymake-jshint flymake-cursor flycheck-rust flycheck-inline fill-column-indicator eyebrowse ess erc-hl-nicks eproject elpy elisp-slime-nav edit-server dumb-jump dtrt-indent dired-details diminish counsel-pydoc counsel-projectile counsel-org-clock counsel-gtags counsel-etags company-racer color-moccur cmake-mode change-inner browse-kill-ring blackboard-theme auto-indent-mode auctex android-mode ace-window ace-jump-mode ac-nrepl)))
 '(safe-local-variable-values
   (quote
    ((eval c-set-offset
           (quote innamespace)
           0)
     (eval when
           (fboundp
            (quote c-toggle-comment-style))
           (c-toggle-comment-style 1))
     (org-latex-minted-options
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
