(require 'package)

;; we need all this setup here instead of in config.org, because we
;; need to update org-mode instead of using the packaged version.

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; temp hack to allow emacs to fetch https from gnu.org
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (message (format "use-package installed: %s" (package-installed-p 'use-package)))
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

(use-package org
  :ensure org
  :pin gnu)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
(put 'list-timers 'disabled nil)
