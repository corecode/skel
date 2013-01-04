(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global nil
      truncate-partial-width-windows nil)

;; theme
(load-theme 'blackboard t)

;; modify modeline
(face-spec-set 'mode-line '((t (:background "darkred" :foreground "white" :box (:line-width -1 :style released-button)))))

(defun setup-frame-defaults (&optional frame)
  ;; default font
  ;; (set-frame-font "Anonymous Pro 11" t t)
  ;; (set-frame-font "Inconsolata 13" t t)
  ;; (set-frame-font "Droid Sans Mono-11" t t)
  (set-default-font "Liberation Mono 11" t t)
  (remove-hook 'after-make-frame-functions 'setup-frame-defaults))

;; else tty - fix up background
;;(face-spec-set 'default '((t (:background "black"))) (selected-frame))
(add-hook 'after-make-frame-functions 'setup-frame-defaults)
;; (add-hook 'after-init-hook 'setup-frame-defaults)

(set-face-background 'region "#464740")

;; Highlight current line
(global-hl-line-mode 1)

;; Customize background color of lighlighted line
(set-face-background 'hl-line "#222222")

;; Highlight in yasnippet
(set-face-background 'yas-field-highlight-face "#333399")

(set-face-foreground 'font-lock-warning-face "#ff6666")

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

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (turn-off-tool-bar)
  (tooltip-mode -1)
  (turn-off-tool-bar)
  (blink-cursor-mode -1))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Ditch them scrollbars
(scroll-bar-mode -1)

;; ;; Make zooming affect frame instead of buffers
;; (require 'zoom-frm)

(provide 'appearance)
