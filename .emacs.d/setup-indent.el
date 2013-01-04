(require 'auto-indent-mode)

(setq auto-indent-untabify-on-save-file nil
      auto-indent-blank-lines-on-move nil
      auto-indent-kill-line-kill-region-when-active nil
      auto-indent-next-pair nil
      auto-indent-current-pairs nil)

;; make M-RET like RET, just without breaking the current line
;; (setq-default auto-indent-key-for-end-of-line-then-newline "<M-return>")
;; (setq-default auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")

(auto-indent-global-mode 1)

;; default to indent 8, indent with tabs
(setq standard-indent 8
      tab-always-indent t
      indent-tabs-mode t
      backward-delete-char-untabify-method nil)

(provide 'setup-indent)
