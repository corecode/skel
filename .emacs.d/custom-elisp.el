(defun custom-emacs-lisp-hook ()
  "Customize emacs-lisp-mode."
  (interactive)
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook 'custom-emacs-lisp-hook)
