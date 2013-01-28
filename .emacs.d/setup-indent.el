;; (require 'auto-indent-mode)

;; (setq auto-indent-untabify-on-save-file nil
;;       auto-indent-blank-lines-on-move nil
;;       auto-indent-kill-line-kill-region-when-active nil
;;       auto-indent-next-pair nil
;;       auto-indent-current-pairs nil
;;       auto-indent-assign-indent-level-variables nil)

;; make M-RET like RET, just without breaking the current line
;; (setq-default auto-indent-key-for-end-of-line-then-newline "<M-return>")
;; (setq-default auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")

;; (auto-indent-global-mode 1)

;; RET = prepare indentation
(global-set-key (kbd "RET") 'newline-and-indent)

;; default to indent 8, only use tabs in certain modes
(setq-default standard-indent 8
              tab-always-indent t
              indent-tabs-mode nil
              backward-delete-char-untabify-method nil)

;; guess tab/offset mode
(require 'dtrt-indent)
(add-to-list 'dtrt-indent-hook-mapping-list
             '(js2-mode c/c++/java    js2-basic-offset))
(dtrt-indent-mode 1)

;; make <tab> do the right thing
(require 'smart-tab)
(diminish 'smart-tab-mode)

;; please use default
(setq smart-tab-using-hippie-expand t)
(setq smart-tab-completion-functions-alist
      (assq-delete-all 'emacs-lisp-mode smart-tab-completion-functions-alist))

;; w3m does not like smart-tab-mode
;; solution from <http://stackoverflow.com/questions/6837511/automatically-disable-a-global-minor-mode-for-a-specific-major-mode>
(define-globalized-minor-mode global-smart-tab-mode
  smart-tab-mode
  (lambda ()
    (unless (eq major-mode 'w3m-mode)
      (smart-tab-mode-on)))
  :group 'smart-tab)

(global-smart-tab-mode 1)

(provide 'setup-indent)
