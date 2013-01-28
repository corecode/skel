;; ;; install directly
;; (condition-case err
;;   (error
;;    (message "Problems installing el-get packages: %s" (error-message-string err))))


;;; custom functions, mostly from esk

;;; These belong in prog-mode-hook:

;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a byte-compiled lambda doesn't already exist in the list.

(defun esk-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun esk-turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun esk-turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun esk-turn-on-whitespace ()
  (whitespace-mode t))

(defun esk-turn-on-paredit ()
  (paredit-mode t))

(defun esk-turn-on-idle-highlight-mode ()
  (idle-highlight-mode t))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(XXX\\|FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-local-column-number-mode)
(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'esk-turn-on-save-place-mode)
(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)
(add-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

(defun esk-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(defun esk-turn-off-tool-bar ()
  (tool-bar-mode -1))

(defun esk-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun esk-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun esk-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (esk-indent-buffer)
  (esk-untabify-buffer)
  (delete-trailing-whitespace))

;; Commands

(require 'recentf)
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun esk-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun esk-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun esk-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun esk-suck-it (suckee)
  "Insert a comment of appropriate length about what can suck it."
  (interactive "MWhat can suck it? ")
  (let ((prefix (concat ";; " suckee " can s"))
        (postfix "ck it!")
        (col (current-column)))
    (insert prefix)
    (dotimes (_ (- 80 col (length prefix) (length postfix))) (insert "u"))
    (insert postfix)))

(defun esk-insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun esk-pairing-bot ()
  "If you can't pair program with a human, use this instead."
  (interactive)
  (message (if (y-or-n-p "Do you have a test for that? ") "Good." "Bad!")))

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))


(unless (boundp 'orig-called-interactively-p)
  (defvar orig-called-interactively-p (symbol-function 'called-interactively-p)))


(setq-default fixme-password "")
(condition-case ex
    (load (concat dotfiles-dir "passwords-nocommit.el"))
  ('error))


;; (mapc (lambda (pair)
;;         (put (car pair) 'safe-local-variable (cdr pair)))
;;       '((espresso-indent-level . integerp)))


;; ;; make raise-frame work correctly
;; ;; from: <http://article.gmane.org/gmane.emacs.devel:39702>
;; (defadvice raise-frame (after make-it-work (&optional frame) activate)
;;   "Make it work."
;;   (call-process
;;    "wmctrl" nil nil nil "-i" "-R"
;;    (frame-parameter (or frame (selected-frame)) 'outer-window-id)))

(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)


(require 'find-func)

(eval-after-load 'yasnippet
  (lambda ()
    (add-to-list 'yas/snippet-dirs (concat (file-name-directory (find-library-name "yasnippet"))
                                           "snippets"))
    (let ((snipdir "~.emacs.d/snippets"))
      (when (file-directory-p snipdir)
        (add-to-list 'yas/snippet-dirs snipdir nil))) ; prepend to list

    (yas/reload-all)
    (yas/global-mode 1)))

(setq yas/trigger-key "TAB")


;; setup auto-complete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; (define-key ac-completing-map [tab] 'ac-complete)
;; (define-key ac-completing-map [return] nil)
;; (define-key ac-menu-map [return] 'ac-complete)
;; (define-key ac-menu-map (kbd "RET") 'ac-complete)

;; ;; (setq ac-auto-show-menu 2)

;; ;; (defun my-ac-c-mode-hook ()
;; ;;   )

;; ;; (add-hook 'c-mode-common-hook 'my-ac-c-mode-hook)


;; (require 'yasnippet)

;; (defvar yas-candidates nil)
;; (make-variable-buffer-local 'yas-candidates)

;; (defun init-yas-candidates ()
;;   (let ((table (yas/get-snippet-tables major-mode)))
;;     (if table
;;         (let (candidates (list))
;;           (mapcar (lambda (mode)
;;                     (maphash (lambda (key value)
;;                                (push key candidates))
;;                              (yas/table-hash mode)))
;;                   table)
;;           (setq yas-candidates candidates)))))


;; (defvar ac-new-yas-source
;;   '(    (init . init-yas-candidates)
;;         (candidates . yas-candidates)
;;         (action . yas/expand)
;;         (symbol . "a")))


;; (setq-default ac-sources
;;               (append '(ac-new-yas-source ac-source-semantic) ac-sources))


;; ;;
;; ;; To use in a given major-mode, e.g., js-mode, use:
;; ;;              (add-hook 'js-mode-hook (lambda () (yas/minor-mode-on)))
;; ;;              (add-hook 'js-mode-hook (lambda () (add-to-list 'ac-sources `ac-new-yas-source)))
;; ;;
;; ;; Works best with the following:
;; ;;              (define-key ac-complete-mode-map "\t" 'ac-complete)
;; ;;              (define-key ac-complete-mode-map "\r" nil)
;; ;;              (setq yas/trigger-key "TAB")
;; ;;


;; color theme handling: tweak color theme for tty frames
;; otherwise gnome-terminal sets a weird background color.
;; this also sets the default font for X frames

;; (require 'color-theme)
(setq-default color-theme-is-global nil)
(defun frame-select-color-theme (&optional frame)
  "Set the right color theme for a new frame."
  (let* ((frame (or frame (selected-frame))))
    (select-frame frame)

    ;; default color theme
    (color-theme-blackboard)
    ;; modify modeline
    (face-spec-set 'mode-line '((t (:background "darkred" :foreground "white" :box (:line-width -1 :style released-button)))))

    (if (and (fboundp 'window-system)
             (window-system))
      ;; default font
      (set-default-font "Terminus-10")

      ;; else tty - fix up background
     (face-spec-set 'default '((t (:background "black"))) (selected-frame)))))
(add-hook 'after-make-frame-functions 'frame-select-color-theme)

;; currently broken with bzr emacs
;; ;; set color theme for this frame
;; (if (selected-frame)
;;     (frame-select-color-theme))

(require 'whitespace)
(global-whitespace-mode t)

;; save history across invocations
(setq-default savehist-additional-variables
              '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; needed for auto-indent-mode
(require 'shrink-whitespaces)

;; disable messages for flyspell (also unbreaks flyspell on Ubuntu 10.10)
(setq-default flyspell-issue-welcome-flag nil
              flyspell-issue-message-flag nil)


(setq-default browse-url-browser-function 'browse-url-generic
              browse-url-generic-program "xdg-open")


(defun custom-emacs-lisp-hook ()
  "Customize emacs-lisp-mode."
  (interactive)
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook 'custom-emacs-lisp-hook)


(require 'hyde)
(setq hyde-home "~/Documents/hp/")


(setq-default TeX-master 'dwim)
(setq-default TeX-auto-save t
              TeX-parse-self t
              reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (visual-line-mode t)
                             (flyspell-mode t)
                             (LaTeX-math-mode t)
                             (turn-on-reftex)
                             (TeX-PDF-mode t)))

(setq reftex-label-alist
      '(("compactenum" ?i "item:" nil item nil)))


(add-to-list 'auto-mode-alist '("\\.mkdn\\'" . markdown-mode))

(defun markdown-mode-map-tab-right ()
  "Fix markdown-mode tab mapping."
  (let ((old-key (lookup-key markdown-mode-map (kbd "<tab>"))))
    (when old-key
      (define-key markdown-mode-map (kbd "TAB") old-key)))
  (define-key markdown-mode-map (kbd "<tab>") nil))

(add-hook 'markdown-mode-hook 'markdown-mode-map-tab-right)


;; (require 'mediawiki)

(setq mediawiki-site-alist
      `(
        ("wikipedia" "http://en.wikipedia.org/w/" "username" "password" "Main Page")
        ("fixme" "https://fixme.ch/w/" "corecode" ,fixme-password "User:Corecode")))


(setq-default org-startup-indented t)

(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))


(defun custom-ruby-hook ()
  "Customize ruby-mode."
  (interactive)
  (setq indent-tabs-mode nil)
  (define-key ruby-mode-map (kbd "M-l") 'insert-hashrocket))

(defun insert-hashrocket ()
  "Inserts a hashrocket at point."
  (interactive)
  (unless (or (looking-at "\\s*=>\\s*") (looking-back "\\s*=>\\s*"))
    (delete-horizontal-space)
    (insert " => ")))

(add-hook 'ruby-mode-hook 'custom-ruby-hook)


(setq-default sh-basic-offset 8
              sh-indent-for-case-label 0
              sh-indent-for-case-alt '+)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'esk-prog-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

;;; Enhance Lisp Modes

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)


(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(\\<fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u0192"
                                                           'decompose-region)))))))
(add-hook 'clojure-mode-hook 'esk-pretty-fn)
(add-hook 'clojurescript-mode-hook 'esk-pretty-fn)

;;; from: http://www.emacswiki.org/emacs/TrampMode
;;; Opens file as sudo/root on C-x C-r

(require 'ido)

(defun find-alternative-file-with-sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (if (string-match "^/sudo:root@localhost:" fname)
          (setq fname (replace-regexp-in-string
                       "^/sudo:root@localhost:" ""
                       fname))
        (setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))

(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)


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

;; (require 'yari)
(global-set-key (kbd "C-h r") 'yari)

(if (boundp 'smex)
    (global-set-key (kbd "M-x") 'smex)
  )

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-c x") 'execute-extended-command)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'esk-eval-and-replace)

;; M-S-6 is awkward
(global-set-key (kbd "C-c q") 'join-line)

;; So good!
(global-set-key (kbd "C-x g") 'magit-status)

;; This is a little hacky since VC doesn't support git add internally
(eval-after-load 'vc
  (define-key vc-prefix-map "i"
    '(lambda () (interactive)
       (if (not (eq 'Git (vc-backend buffer-file-name)))
           (vc-register)
         (shell-command (format "git add %s" buffer-file-name))
         (message "Staged changes.")))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))



;; A monkeypatch to cause annotate to ignore whitespace
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; can't do it at launch or emacsclient won't always honor it
(add-hook 'before-make-frame-hook 'esk-turn-off-tool-bar)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 100))

;; Set this to whatever browser you use
;; (setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-default-macosx-browser)
;; (setq browse-url-browser-function 'browse-default-windows-browser)
;; (setq browse-url-browser-function 'browse-default-kde)
;; (setq browse-url-browser-function 'browse-default-epiphany)
;; (setq browse-url-browser-function 'browse-default-w3m)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "~/src/conkeror/conkeror")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(random t) ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))

;; Add this back in at the end of the list.
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

;; Cosmetics

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; Get around the emacswiki spam protection
(eval-after-load 'oddmuse
  (add-hook 'oddmuse-mode-hook
            (lambda ()
              (unless (string-match "question" oddmuse-post)
                (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post))))))


(require 'ess-knitr)


(require 'desktop)
(setq desktop-globals-to-save '(desktop-missing-file-warning))
;; (desktop-save-mode 1)
;;; From <http://www.emacswiki.org/emacs/DeskTop>
(defun my-desktop-autosave ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  ;; (if (eq (desktop-owner) (emacs-pid))
  ;;     )
  (if desktop-dirname
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-autosave)

(require 'session)
(add-hook 'after-init-hook 'session-initialize t)


(setq-default visible-bell t
              inhibit-startup-message t
              color-theme-is-global t
              shift-select-mode nil
              mouse-yank-at-point t
              uniquify-buffer-name-style 'forward
              whitespace-style '(face trailing lines-tail space-before-tab)
              whitespace-line-column 100
              ediff-window-setup-function 'ediff-setup-windows-plain
              oddmuse-directory (concat dotfiles-dir "oddmuse")
              save-place-file (concat dotfiles-dir "places")
              backup-directory-alist `(("." . , (expand-file-name (concat dotfiles-dir "backups"))))
              diff-switches "-u")


;; start the emacs server
(server-start)
