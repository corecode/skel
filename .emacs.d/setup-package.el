(require 'package)

(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")
	;("marmalade" . "http://marmalade-repo.org/packages/")
	("ELPA" . "http://tromey.com/elpa/")
        ("org" . "http://orgmode.org/elpa/")))

;; load elpa repos if we don't have a copy yet
(package-initialize)
(package-read-all-archive-contents)
(unless package-archive-contents
  (package-refresh-contents))

(defun package-install-if-missing (&rest packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (condition-case err
	  (package-install package)
	(error				; condition
	 (warn "Problems installing package `%s': %s" package (error-message-string err)))))))

(provide 'setup-package)
