;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'nrepl-mode-hook 'enable-paredit-mode)

(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode 1)))
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(diminish 'eldoc-mode)

;; dim + colored parens
(dolist (mode '(clojure-mode-hook
                emacs-lisp-mode-hook
                ielm-mode-hook
                inferior-jess-mode-hook
                jess-mode-hook
                lisp-interaction-mode-hook
                lisp-mode-hook
                nrepl-mode-hook
                scheme-mode-hook
                slime-repl-mode-hook))
  (add-hook mode 'rainbow-delimiters-mode-enable))


(provide 'setup-lisp-mode)
