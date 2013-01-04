(require 'desktop)

(add-to-list 'desktop-globals-to-save 'kill-ring)
(add-to-list 'desktop-globals-to-save 'search-ring)
(add-to-list 'desktop-globals-to-save 'regexp-search-ring)
;; (desktop-save-mode 1)

;;; From <http://www.emacswiki.org/emacs/DeskTop>

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(defun my-desktop-autosave ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  ;; (if (eq (desktop-owner) (emacs-pid))
  ;;     )
  (if desktop-dirname
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-autosave)

(savehist-mode 1)

(provide 'setup-desktop)
