(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

;; temp hack to allow emacs to fetch https from gnu.org
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-enable-at-startup nil)
(package-initialize)

(use-package org
  :ensure org-plus-contrib
  :pin org)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
