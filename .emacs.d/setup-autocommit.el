(defvar 2c-vc-autocommit)
(make-variable-buffer-local '2c-vc-autocommit)
(put '2c-vc-autocommit 'safe-local-variable 'booleanp)

(add-to-list 'after-save-hook '2c-maybe-vc-autocommit)

(defun 2c-maybe-vc-autocommit ()
  "Auto commit to git after saving, if `2c-git-autocommit' is t."
  (if (and (boundp '2c-vc-autocommit)
           2c-vc-autocommit
           (vc-registered buffer-file-name))
      (let* ((backend (vc-responsible-backend buffer-file-name))
             (rootdir (vc-call-backend backend 'root default-directory))
             (commitmsg
              (if (and (fboundp '2c-vc-autocommit-message)
                       2c-vc-autocommit-message)
                  (2c-vc-autocommit-message)
                (format "auto-commit %s"
                        (file-relative-name buffer-file-name rootdir)))))
        (vc-checkin
         (list buffer-file-name)
         backend
         nil
         commitmsg))))

(provide 'setup-autocommit)
