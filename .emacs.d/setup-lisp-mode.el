;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

(add-hook 'scheme-mode 'turn-on-geiser-mode)

(require 'nrepl)

(dolist (mode '(lisp-mode-hook
                emacs-lisp-mode-hook
                clojure-mode-hook
                nrepl-repl-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook
                geiser-repl-mode-hook))
  (add-hook mode 'enable-paredit-mode))

(define-key nrepl-interaction-mode-map (kbd "C-c v") 'nrepl-eval-buffer)
(define-key nrepl-interaction-mode-map (kbd "TAB") 'nrepl-tab)

(defun nrepl-goto-prompt ()
  (interactive)
  (goto-char nrepl-input-start-mark))
(define-key nrepl-repl-mode-map (kbd "C-a") 'nrepl-goto-prompt)
(define-key nrepl-repl-mode-map (kbd "C-S-k") 'nrepl-delete-current-input)

(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode 1)))
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(diminish 'eldoc-mode)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;; dim + colored parens
(dolist (mode '(clojure-mode-hook
                emacs-lisp-mode-hook
                ielm-mode-hook
                inferior-jess-mode-hook
                jess-mode-hook
                lisp-interaction-mode-hook
                lisp-mode-hook
                nrepl-repl-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook
                slime-repl-mode-hook))
  (add-hook mode 'rainbow-delimiters-mode-enable))


(provide 'setup-lisp-mode)
