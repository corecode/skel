;; below automatically added settings by custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
 '(auto-indent-next-pair-timer-interval (quote ((default 0.0005))))
 '(erc-modules (quote (completion move-to-prompt stamp spelling truncate hl-nicks netsplit fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt menu list scroll-conservatively)))
 '(org-agenda-files (quote ("~/.org/notes.org" "~/.org/todo.org")))
 '(org-log-into-drawer t)
 '(safe-local-variable-values (quote ((eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode)) (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t) (ada-prj-default-comp-opt . "-gnatq -gnatQ -gnat2012") (noweb-default-code-mode . R-mode) (whitespace-line-column . 100) (c-default-style . gnu) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby") (lexical-binding . t))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "0x2c.org")
 '(smtpmail-smtp-service 587)
 '(smtpmail-smtp-user "2")
 '(smtpmail-stream-type (quote starttls)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
