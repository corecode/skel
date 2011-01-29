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

;; load all other custom-*.el files
(mapc #'load (directory-files dotfiles-dir nil "^custom-.*[.]el$"))

;; start the emacs server
(server-start)
