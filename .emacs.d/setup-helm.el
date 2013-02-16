(require 'helm-config)
(require 'helm-etags+)

(global-set-key (kbd "M-.") 'helm-etags+-select)
(global-set-key (kbd "M-*") 'helm-etags+-history)
(global-set-key (kbd "M-,") 'helm-etags+-history-go-back)
(global-set-key (kbd "M-/") 'helm-etags+-history-go-forward)
