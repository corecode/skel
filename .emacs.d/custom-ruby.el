(defun custom-ruby-hook ()
  "Customize ruby-mode."
  (interactive)
  (setq indent-tabs-mode nil))

(add-hook 'ruby-mode-hook 'custom-ruby-hook)
