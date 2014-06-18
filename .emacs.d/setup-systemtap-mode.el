(add-hook 'systemtap-mode-hook (lambda () (set (make-local-variable 'comment-start) "// ")))

(provide 'setup-systemtap-mode)
