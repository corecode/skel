(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global nil
      truncate-partial-width-windows nil)

;; theme
(load-theme 'blackboard t)

;; modify modeline
(face-spec-set 'mode-line '((t (:background "darkred" :foreground "white" :box (:line-width -1 :style released-button)))))

(set-face-background 'region "#464740")

;; Highlight current line
(global-hl-line-mode 1)

;; Customize background color of lighlighted line
(set-face-background 'hl-line "#222222")

;; Highlight in yasnippet
;;(set-face-background 'yas-field-highlight-face "#333399")

(set-face-foreground 'font-lock-warning-face "#ff6666")

;; use fancy mode-line
(require 'powerline)
(powerline-default)

;; default font
(setq default-frame-alist (assq-delete-all 'font default-frame-alist))
(setq default-frame-alist (delq (assoc 'font default-frame-alist) default-frame-alist))
(add-to-list 'default-frame-alist '(font . "Liberation Mono-10:weight=normal"))
;; "Anonymous Pro 11"
;; "Inconsolata 13"
;; "Droid Sans Mono-11"
;; "Liberation Mono 11"

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(XXX\\|FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; No menu bars
(menu-bar-mode -1)

;; (when window-system)
(setq frame-title-format
      '(:eval
        (if (buffer-file-name)
            (abbreviate-file-name (buffer-file-name))
          "%b")))
(turn-off-tool-bar)
(tooltip-mode -1)
(turn-off-tool-bar)
(blink-cursor-mode -1)

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Ditch them scrollbars
(scroll-bar-mode -1)

;; Diminish modeline clutter
(eval-after-load 'eproject
  '(diminish 'eproject-mode))
(eval-after-load 'abbrev
  '(diminish 'abbrev-mode))
;; (diminish 'auto-fill-mode)

;; ;; Make zooming affect frame instead of buffers
;; (require 'zoom-frm)

(provide 'appearance)
