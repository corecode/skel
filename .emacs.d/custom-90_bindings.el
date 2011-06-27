;; set f11 as full screen toggle
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
(global-set-key [f11] 'toggle-fullscreen)

;; default indent after RET
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq-default auto-indent-indentation-function 'newline-and-indent)

;;(setq-default auto-indent-key-for-end-of-line-then-newline "<M-return>")
(setq-default auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")

;; make M-RET like RET, just without breaking the current line
(defun insert-newline-at-end ()
  (interactive)
  (move-end-of-line nil)
  (funcall (key-binding (kbd "RET"))))
(global-set-key (kbd "<M-return>") 'insert-newline-at-end)

;; kill whole line
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; make C-* mimic vim's `*' search
(defun vim-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun vim-isearch-yank-word-hook ()
  (when (equal this-command 'vim-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (save-excursion (skip-syntax-backward "w_") (point))
                           (save-excursion (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (setq isearch-case-fold-search nil ; search is case-sensitive
            isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'vim-isearch-yank-word-hook)
(define-key global-map (kbd "C-*") 'vim-isearch-word-at-point)

;; FIXME: maybe move to custom-ruby.el?
(require 'yari)
(global-set-key (kbd "C-h r") 'yari)

;; hook up man
(define-key help-map (kbd "C-m") 'man)
(define-key help-map (kbd "M") 'man)
