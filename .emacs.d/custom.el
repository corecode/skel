;; make all subdirs of ~/.emacs.d/vendor loadable
;; however, sort the emacswiki.org directory to the end.
(let* ((default-directory (concat dotfiles-dir "vendor")))
  (when (file-exists-p default-directory)
    (progn
      (add-to-list 'load-path default-directory)
      (let* ((subdirs (remove-if-not 'file-directory-p (directory-files default-directory)))
	     (filter (lambda (p) (string-match "emacswiki\\.org" p)))
	     (frontpart (remove-if (lambda (p) (or (funcall filter p)
					      (member p '("." "..")))) subdirs))
	     (tailpart (remove-if-not filter subdirs)))
	(dolist (dir frontpart)
	  (add-to-list 'load-path (expand-file-name dir)))
	(dolist (dir tailpart)
	  (add-to-list 'load-path (expand-file-name dir) t))))))

(let ((local-dir (concat dotfiles-dir "local")))
  (when (file-directory-p local-dir)
    (add-to-list 'load-path local-dir nil)))

;; load all other custom-*.el files
(mapc #'load (directory-files dotfiles-dir nil "^custom-.*[.]el$"))

;; start the emacs server
(server-start)

;; below automatically added settings by custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((c-default-style . gnu) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby") (whitespace-line-column . 80) (lexical-binding . t)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
