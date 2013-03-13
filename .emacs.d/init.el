;; Much of the following has been adopted (or plainly copied) from the
;; Emacs-Starter-Kit by Phil Hagelberg <https://github.com/technomancy/emacs-starter-kit/>
;; The Emacs-Starter-Kit is licenced under the GPLv3.  Refer to the URL above for details.
;; More adaptation from <https://github.com/magnars/.emacs.d>

(progn
  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(setq inhibit-startup-message t)

;; Set path to dependencies
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq site-lisp-dir
      (expand-file-name "site-lisp" dotfiles-dir))

;; set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; same for themes
(setq site-theme-dir
      (expand-file-name "themes" dotfiles-dir))
(add-to-list 'custom-theme-load-path site-theme-dir)
(dolist (dir (directory-files site-theme-dir t "\\w+"))
  (when (file-directory-p dir)
    (add-to-list 'custom-theme-load-path dir)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat dotfiles-dir "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" dotfiles-dir))


;;; packages
(require 'setup-package)

(setq my:packages
      '(
        auctex
        ac-slime
        ace-jump-mode
        auto-complete
        auto-indent-mode
        browse-kill-ring
        ;; csharp-mode
        ;; coffee-mode
        change-inner
        clojure-mode
        clojure-project-mode
        color-moccur
        dash
        diminish
        dired-details
        elisp-slime-nav
        ;; emacs-goodies-el
        eproject
        ess
        expand-region
        fill-column-indicator
        find-file-in-project
        flymake-cursor
        flymake-ruby
        fold-this
        ;; frame-fns
        ;; frame-cmds
        gist
        git-commit-mode
        gitconfig-mode
        gitignore-mode
        goto-chg
        haskell-mode
        helm
        helm-gist
        highlight-indentation
        ;; idle-highlight-mode
        ido-ubiquitous
        iedit
        inf-ruby
        json
        js2-mode
        js2-refactor
        jump-char
        key-chord
        magit
        magit-gh-pulls
        magit-push-remote
        mark-multiple
        markdown-mode
        mediawiki
        move-text
        multi-term
        multifiles
        multiple-cursors
        nrepl
        ;; oddmuse
        paredit
        ;; php-mode
        ;; rinari
        rainbow-delimiters
        ruby-compilation
        ruby-block
        ruby-end
        ruby-mode
        rvm
        powerline
        python-mode                     ; needs to be loaded after
                                        ; ruby-*, or ruby-mode will break
        quack
        s
        session
        slime
        smart-tab
        smart-forward
        smex
        smooth-scrolling
        top-mode
        ;; tuareg-mode
        undo-tree
        w3m
        wgrep
        yari
        yaml-mode
        yasnippet
        zoom-frm
        ))

(apply 'package-install-if-missing my:packages)

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; mode setups will need this later
(require 'diminish)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(require 'setup-hippie)
(require 'setup-yasnippet)
(require 'setup-ffip)
(require 'setup-paredit)
(require 'setup-indent)
(require 'setup-term)
(require 'setup-helm)
(require 'setup-tex)

;; Language specific setup files
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))
(eval-after-load 'cc-mode '(require 'setup-cc-mode))
(eval-after-load 'lisp-mode '(require 'setup-lisp-mode))

;; ;; Load slime-js when asked for
;; (autoload 'slime-js-jack-in-browser "setup-slime-js" nil t)
;; (autoload 'slime-js-jack-in-node "setup-slime-js" nil t)

;; Map files to modes
(require 'mode-mappings)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'inline-string-rectangle)
(require 'multiple-cursors)
(require 'delsel)
(require 'jump-char)
(require 'eproject)
(require 'wgrep)
(require 'smart-forward)
(require 'change-inner)
(require 'multifiles)
(require 'git-modeline)
(require 'goto-chg)

;; ;; Fill column indicator
;; (require 'fill-column-indicator)
;; (setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; Misc
(require 'appearance)
(require 'my-misc)
(require 'setup-auth)

(require 'setup-desktop)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
