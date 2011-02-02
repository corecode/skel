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
