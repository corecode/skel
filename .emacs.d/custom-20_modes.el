;; show trailing whitespace
(setq-default whitespace-style
	      '(trailing
		space-before-tab))
(require 'whitespace)
(global-whitespace-mode t)

;; save history across invocations
(setq-default savehist-additional-variables
	      '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; setup auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; needed for auto-indent-mode
(require 'shrink-whitespaces)
;; automatically indent
(setq-default auto-indent-untabify-on-save-file nil
              auto-indent-blank-lines-on-move nil)
(require 'auto-indent-mode)
(auto-indent-global-mode 1)

;; default to indent 8, indent with tabs
(setq-default standard-indent 8
	      tab-always-indent t
	      indent-tabs-mode t
	      backward-delete-char-untabify-method nil)

;; disable messages for flyspell (also unbreaks flyspell on Ubuntu 10.10)
(setq-default flyspell-issue-welcome-flag nil
	      flyspell-issue-message-flag nil)
