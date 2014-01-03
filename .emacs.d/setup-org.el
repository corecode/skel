(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (ignore-errors
        (save-excursion
          (org-back-to-heading)
          (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(setq org-default-notes-file "~/.org/notes.org")

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n  %u\n  %a")
        ("n" "Note" entry (file+headline "~/.org/notes.org" "Notes") "* %?\n  %u\n  %a")))

(define-key org-mode-map (kbd "C-'") nil)

(provide 'setup-org)
