(defun ruby--jump-to-test ()
  (find-file
   (replace-regexp-in-string
    "/lib/" "/test/"
    (replace-regexp-in-string
     "/\\([^/]+\\).rb$" "/test_\\1.rb"
     (buffer-file-name)))))

(defun ruby--jump-to-lib ()
  (find-file
   (replace-regexp-in-string
    "/test/" "/lib/"
    (replace-regexp-in-string
     "/test_\\([^/]+\\).rb$" "/\\1.rb"
     (buffer-file-name)))))

(defun ruby-jump-to-other ()
  (interactive)
  (if (string-match-p "/test/" (buffer-file-name))
      (ruby--jump-to-lib)
    (ruby--jump-to-test)))

(defun insert-hashrocket ()
  "Inserts a hashrocket at point."
  (interactive)
  (unless (or (looking-at "\\s*=>\\s*") (looking-back "\\s*=>\\s*"))
    (delete-horizontal-space)
    (insert " => ")))

(define-key ruby-mode-map (kbd "M-l") 'insert-hashrocket)
(define-key ruby-mode-map (kbd "C-c t") 'ruby-jump-to-other)
(define-key ruby-mode-map (kbd "C-h r") 'yari-helm)

(add-hook 'ruby-mode-hook 'ruby-end-mode)

(provide 'setup-ruby-mode)
