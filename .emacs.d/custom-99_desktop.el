(require 'session)

(setq desktop-globals-to-save '(desktop-missing-file-warning))
(desktop-save-mode 1)

(add-hook 'after-init-hook 'session-initialize)
