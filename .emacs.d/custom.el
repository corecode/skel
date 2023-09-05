;; below automatically added settings by custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-indent-next-pair-timer-geo-mean '((default 0.0005 0)))
 '(auto-indent-next-pair-timer-interval '((default 0.0005)))
 '(custom-safe-themes
   '("f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default))
 '(doc-view-resolution 200)
 '(godoc-command "godoc")
 '(gofmt-command "goimports")
 '(jabber-account-list '(("2@0x2c.org")))
 '(jabber-otr-message-history t)
 '(nrepl-popup-stacktraces t)
 '(nrepl-popup-stacktraces-in-repl t)
 '(org-agenda-files '("~/.org/notes.org" "~/.org/todo.org"))
 '(org-journal-file-format "%Y%m")
 '(org-log-into-drawer t)
 '(org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")))
 '(org-time-clocksum-format
   '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
 '(safe-local-variable-values
   '((eval add-to-list 'lsp-file-watch-ignored-directories "gecko_sdk")
     (eval add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]flowos-vision3[/\\\\]src[/\\\\]gecko_sdk")
     (cov-lcov-file-name . "converage/lcov.info")
     (eval c-set-offset 'innamespace 0)
     (eval when
	   (fboundp 'c-toggle-comment-style)
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
	   (add-hook 'write-contents-functions
		     (lambda nil
		       (delete-trailing-whitespace)
		       nil))
	   (require 'whitespace)
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
     (lexical-binding . t)))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "0x2c.org")
 '(smtpmail-smtp-service 587)
 '(smtpmail-smtp-user "2")
 '(smtpmail-stream-type 'starttls)
 '(verilog-auto-endcomments nil)
 '(verilog-auto-lineup 'all)
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
