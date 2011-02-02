(require 'multi-term)

(setq term-unbind-key-list
      '("M-x"
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
        ;; ("C-p" . previous-line)
        ;; ("C-n" . next-line)
        ("C-c C-s" . isearch-forward)
        ("C-c C-r" . isearch-backward)
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

;; don't switch to other terminals
(setq multi-term-switch-after-close nil)

;; match my prompt
(setq term-prompt-regexp "^\\([0-9]+ \\)[%#] ")

(define-key global-map (kbd "C-t") 'multi-term)

;; remove SHLVL.  For any shell, we're the top level
(setenv "SHLVL" nil)
