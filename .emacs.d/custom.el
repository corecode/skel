(setq starter-kit-packages (list 'kill-ring-search
				 'lisppaste
				 'xml-rpc
				 'org
				 'paredit
				 'rinari
				 'jump
				 'inflections
				 'findr
				 'ruby-compilation
				 'c-eldoc
				 'css-mode
				 'guess-style
				 'highlight-symbol
				 'htmlize
				 'http-post-simple
				 'auto-dictionary
				 'Save-visited-files
				 'slime
				 'smart-operator
				 'smex
				 'textmate
				 'yasnippet-bundle
				 'auctex
				 'find-file-in-project
				 'gist
				 'idle-highlight
				 'inf-ruby
				 'magit
				 'project-local-variables
				 'ruby-mode
				 'yaml-mode
				 'yari))

;; make all subdirs of ~/.emacs.d/vendor loadable
;; however, sort the emacswiki.org directory to the end.
(let* ((default-directory (concat dotfiles-dir "vendor")))
  (when (file-exists-p default-directory)
    (progn
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path)
      (let* ((filter '(lambda (p) (string-match "/emacswiki\\.org" p)))
	     (tailpart (remove-if-not filter load-path)))
	(delete-if filter load-path)
	(mapc '(lambda (p) (add-to-list 'load-path p t)) tailpart)))))

;;; version like above, just adds directories to the end of load-path
;; (let* ((default-directory (concat dotfiles-dir "vendor")))
;;   (when (file-exists-p default-directory)
;;       (progn
;;         (add-to-list 'load-path default-directory t)
;; 	(mapc #'(lambda (full-name)
;; 		  (let* ((dir-name (file-name-nondirectory full-name)))
;; 		    (when (and (file-directory-p full-name)
;; 			       (not (string= dir-name "."))
;; 			       (not (string= dir-name "..")))
;; 		      (add-to-list 'load-path full-name t))))
;; 	      (directory-files default-directory t)))))

;; color theme handling: tweak color theme for tty frames
;; otherwise gnome-terminal sets a weird background color.
;; this also sets the default font for X frames
(require 'color-theme)
(setq color-theme-is-global nil)
(defun frame-select-color-theme (&optional frame)
  "Set the right color theme for a new frame."
  (let* ((frame (or frame (selected-frame))))
    (select-frame frame)
    ;; default color theme
    (color-theme-blackboard)
    (if (and (fboundp 'window-system)
	     (window-system))

	;; default font
	(set-default-font "Terminus-10")

      ;; else tty - fix up background
      (face-spec-set 'default '((t (:background "black"))) (selected-frame)))))
(add-hook 'after-make-frame-functions 'frame-select-color-theme)

;; set color theme for this frame
(frame-select-color-theme)

;; show trailing whitespace
(setq whitespace-style
      '(trailing
        space-before-tab))
(require 'whitespace)
(global-whitespace-mode t)

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

;; needed for auto-indent-mode
(require 'shrink-whitespaces)
(setq auto-indent-indentation-function 'newline-and-indent)
(setq auto-indent-key-for-end-of-line-then-newline "<M-return>")
(setq auto-indent-key-for-end-of-line-insert-char-then-newline "<M-S-return>")

;; automatically indent
(require 'auto-indent-mode)
(auto-indent-global-mode 1)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; kill whole line
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; default to indent 8, indent with tabs
(setq standard-indent 8
      tab-always-indent t
      indent-tabs-mode t)

(defun custom-set-tabbing-for-mode ()
  "Sets the tab characteristic for a mode"
  (case major-mode
    (('emacs-lisp-mode 'ruby-mode)
     (setq indent-tabs-mode nil))
    (('sh-mode)
     (setq sh-basic-offset 8
	   sh-indent-for-case-label 0
	   sh-indent-for-case-alt "+"))))

(add-hook 'change-major-mode-hook 'custom-set-tabbing-for-mode)

;; disable messages for flyspell (also unbreaks flyspell on Ubuntu 10.10)
(setq flyspell-issue-welcome-flag nil
      flyspell-issue-message-flag nil)

;; latex settings
(setq-default TeX-master 'dwim)
(setq TeX-auto-save t
      TeX-parse-self t
      reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook (lambda ()
			     (visual-line-mode t)
			     (flyspell-mode t)
			     (LaTeX-math-mode t)
			     (turn-on-reftex)
			     (TeX-PDF-mode t)))

;; key bindings
(require 'yari)
(global-set-key (kbd "C-h r") 'yari)


;; make C-* mimic vim's `*' search
(defun vim-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun vim-isearch-yank-word-hook ()
  (when (equal this-command 'vim-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
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

;; start the emacs server
(server-start)
