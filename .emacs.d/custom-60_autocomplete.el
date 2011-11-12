;; setup auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(define-key ac-completing-map "\r" nil)

(defun my-ac-c-mode-hook ()
  (setq ac-sources (append '(ac-source-semantic) ac-sources)))

(add-hook 'c-mode-common-hook 'my-ac-c-mode-hook)
