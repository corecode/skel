(require 'desktop)

(setq desktop-globals-to-save '(desktop-missing-file-warning))
(desktop-save-mode 1)
;;; From <http://www.emacswiki.org/emacs/DeskTop>
(defun my-desktop-autosave ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-autosave)

(require 'session)
(add-hook 'after-init-hook 'session-initialize t)
