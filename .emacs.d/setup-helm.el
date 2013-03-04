(require 'helm-config)
(require 'helm-etags+)

(global-set-key (kbd "M-.") 'helm-etags+-select)
(global-set-key (kbd "M-*") 'helm-etags+-history)
(global-set-key (kbd "M-,") 'helm-etags+-history-go-back)
(global-set-key (kbd "M-/") 'helm-etags+-history-go-forward)

(setq helm-etags+-markers (make-ring 100))

(provide 'setup-helm)
