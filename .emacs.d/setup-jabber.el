(require 'jabber)

(setq jabber-roster-line-format " %c %-25n %u %-8s %S")

(defadvice jabber-chat-buffer-fill-long-lines (around fix-jabber-fill-width activate)
  (let ((fill-column (min fill-column (window-width (get-buffer-window (current-buffer))))))
    ad-do-it))

(add-hook 'jabber-chat-mode-hook 'goto-address-mode)

;;; from <https://github.com/chaoflow/.emacs.d/blob/master/setup-jabber.el>
;; history
(setq jabber-history-enabled t
      ;; jabber-history-muc-enabled t
      jabber-use-global-history nil)

;; colour
(setq jabber-muc-colorize-foreign t
      jabber-muc-colorize-local t
      jabber-muc-participant-colors '(("paper_machine" . "#00006EEEFFFF")
                                      ("MadCow" . "#0000FFFF3FFF")
                                      ("UlliBre" . "#CCCBFFFF0000")
                                      ("libe" . "#D5540000FFFF")
                                      ("MentalFloss" . "#62210000FFFF")
                                      ("daho" . "#00008888FFFF")
                                      ("chaoflow" . "#1999FFFF0000")
                                      ("corecode" . "magenta")
                                      ("tobias" . "#AEEE0000FFFF")))

;; roster
(setq jabber-roster-show-title nil
      jabber-roster-show-bindings nil
      jabber-roster-line-format " %c %-25n %u %-8s  %S"
                                        ; no avatars
      jabber-roster-show-separators nil
      jabber-show-offline-contacts nil
      jabber-show-resources nil)

(set-face-attribute 'jabber-title-large nil :height 1.2)
(set-face-attribute 'jabber-title-medium nil :height 1.0)
(set-face-attribute 'jabber-title-small nil :height 0.8)

;; reconnect
(setq jabber-auto-reconnect t)

;; jabber - muc names binding
(defun bind-jabber-muc-names ()
  (when jabber-group
    (local-set-key (kbd "C-c C-n") 'jabber-muc-names)))
(add-hook 'jabber-chat-mode-hook 'bind-jabber-muc-names)

;; activity
(setq jabber-activity-make-strings 'jabber-activity-make-strings-shorten)
(set-face-attribute 'jabber-activity-personal-face nil :foreground "red")
(set-face-attribute 'jabber-activity-face nil :foreground nil)

;; alerts
(setq jabber-alert-presence-hooks nil)
(callf2 delq 'jabber-message-echo jabber-alert-message-hooks)
(callf2 delq 'jabber-muc-echo jabber-alert-muc-hooks)

(provide 'setup-jabber)
