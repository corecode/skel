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
