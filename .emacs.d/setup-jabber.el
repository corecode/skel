(require 'jabber)

(setq jabber-roster-line-format " %c %-25n %u %-8s %S")

(defadvice jabber-chat-buffer-fill-long-lines (around fix-jabber-fill-width activate)
  (let ((fill-column (min fill-column (window-width (get-buffer-window (current-buffer))))))
    ad-do-it))

(provide 'setup-jabber)
