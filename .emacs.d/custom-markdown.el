(add-to-list 'auto-mode-alist '("\\.mkdn\\'" . markdown-mode))

(defun markdown-mode-map-tab-right ()
  "Fix markdown-mode tab mapping."
  (let ((old-key (lookup-key markdown-mode-map (kbd "<tab>"))))
    (when old-key
      (define-key markdown-mode-map (kbd "TAB") old-key)))
  (define-key markdown-mode-map (kbd "<tab>") nil))

(add-hook 'markdown-mode-hook 'markdown-mode-map-tab-right)
