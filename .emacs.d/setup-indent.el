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
(setq standard-indent 8
      tab-always-indent nil
      indent-tabs-mode t
      backward-delete-char-untabify-method nil)

;; guess tab/offset mode
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; make <tab> do the right thing
(require 'smart-tab)
(diminish 'smart-tab-mode)

;; w3m does not like smart-tab-mode
;; solution from <http://stackoverflow.com/questions/6837511/automatically-disable-a-global-minor-mode-for-a-specific-major-mode>
(define-globalized-minor-mode my-global-smart-tab-mode
  smart-tab-mode
  (lambda ()
    (unless (eq major-mode 'w3m-mode))
    (smart-tab-mode-on))
  :group 'smart-tab)

(my-global-smart-tab-mode 1)

(provide 'setup-indent)
