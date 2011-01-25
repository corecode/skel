(require 'cl)

;; make all subdirs of ~/.emacs.d/ loadable
(let* ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; disable menu bar without X
(if (and (boundp 'window-system))
    (window-system nil)
  (menu-bar-mode -1))
;; disable toolbar
(tool-bar-mode -1)

;; colortheme
(require 'color-theme-2c)
(color-theme-2c)

;; default font
;;(set-default-font "Terminus-10")
(set-default-font "Monospace-10")

;; disable blinking cursor
(blink-cursor-mode -1)

;; show matching parentheses
(show-paren-mode t)

;; show trailing whitespace
(setq whitespace-style
      '(trailing
        space-before-tab))
(require 'whitespace)
(global-whitespace-mode)

;; save history across invocations
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; setup auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; set f11 as full screen toggle
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
(global-set-key [f11] 'toggle-fullscreen)

;; automatically line break in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; needed for auto-indent-mode
(require 'shrink-whitespaces)
(setq auto-indent-key-for-end-of-line-then-newline "<M-return>")
(setq auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")

;; automatically indent
(require 'auto-indent-mode)
(auto-indent-global-mode 1)

(require 'ffap)
(ffap-bindings)
