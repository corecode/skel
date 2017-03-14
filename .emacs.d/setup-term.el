(require 'multi-term)

(setq term-unbind-key-list
      '("M-x"
        "C-c"
        "C-x"
        "C-t"
        "S-<left>"
        "S-<right>"
        "S-<up>"
        "S-<down>"))
;; from multi-term.el, just use C-s and C-p with prefix.
(setq term-bind-key-alist
      '(
        ("C-c C-c" . term-interrupt-subjob)
        ("C-p" . previous-line)
        ("C-n" . next-line)
        ("M-C-s" . isearch-forward)
        ("M-C-r" . isearch-backward)
        ("C-m" . term-send-raw)
        ;; ("M-f" . term-send-forward-word)
        ;; ("M-b" . term-send-backward-word)
        ;; ("M-o" . term-send-backspace)
        ;; ("M-p" . term-send-up)
        ;; ("M-n" . term-send-down)
        ;; ("M-M" . term-send-forward-kill-word)
        ;; ("M-N" . term-send-backward-kill-word)
        ;; ("M-r" . term-send-reverse-search-history)
        ("M-," . term-send-input)
        ("M-." . comint-dynamic-complete)))

(term-set-escape-char ?\C-x)

;; disable minor modes that use C-c
(defun term-disable-interfering-minor-modes ()
  (yas-minor-mode -1)
  (winner-mode -1))
(add-hook 'term-mode-hook 'term-disable-interfering-minor-modes)

;; don't switch to other terminals
(setq multi-term-switch-after-close nil)

;; match my prompt
(setq term-prompt-regexp "^\\([0-9]+ \\)[%#] ")

(setq term-suppress-hard-newline t)

;; term.el is silly and extracts defaults when no frame exists yet
(defun term-setup-default-faces (&optional frame)
  (setq
   term-default-fg-color (face-foreground term-current-face)
   term-default-bg-color (face-background term-current-face))
  (remove-hook 'after-make-frame-functions 'term-setup-default-faces))
(add-hook 'after-make-frame-functions 'term-setup-default-faces)

(defun 2c-rename-term ()
  (let ((bufname (buffer-name))
            (index 1)
            termname
            (newname (lambda () (format "*%s<%d> %s*" termname index default-directory))))

        (set (make-local-variable '2c-prev-default-directory) default-directory)

        (string-match "^[*]?\\(.*?\\)\\(?:<\\(.*?\\)>\\)" bufname)
        (setq termname (match-string 1 bufname))
        (if (match-string 2 bufname)
            (setq index (string-to-number (match-string 2 bufname))))
        (unless (string-equal bufname (funcall newname))
          (while (buffer-live-p (get-buffer (funcall newname)))
            (setq index (1+ index)))
          (rename-buffer (funcall newname)))))

(defadvice term-handle-ansi-terminal-messages (after 2c-update-term-buf-name activate)
  (if (or (not (boundp '2c-prev-default-directory))
          (not (string-equal default-directory 2c-prev-default-directory)))
      (2c-rename-term)))

;; remove SHLVL.  For any shell, we're the top level
(setenv "SHLVL" nil)

(provide 'setup-term)
