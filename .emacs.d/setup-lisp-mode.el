;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

(add-hook 'scheme-mode 'turn-on-geiser-mode)

(require 'cider)

(dolist (mode '(lisp-mode-hook
                emacs-lisp-mode-hook
                clojure-mode-hook
                cider-repl-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook
                geiser-repl-mode-hook))
  (add-hook mode 'enable-paredit-mode))

;; (define-key cider-interaction-mode-map (kbd "C-c v") 'cider-eval-buffer)
;; (define-key cider-interaction-mode-map (kbd "TAB") 'cider-tab)

;; (defun cider-goto-prompt ()
;;   (interactive)
;;   (goto-char cider-input-start-mark))
;; (define-key cider-repl-mode-map (kbd "C-a") 'cider-goto-prompt)
;; (define-key cider-repl-mode-map (kbd "C-S-k") 'cider-delete-current-input)

(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode 1)))
;; (add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
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
                ;;cider-repl-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook
                slime-repl-mode-hook))
  (add-hook mode 'rainbow-delimiters-mode-enable))


(provide 'setup-lisp-mode)
