(require 'org-compat)

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
(setq org-directory "~/.org")
(setq org-mobile-directory "/ssh2:2c:.mobile-org")
(setq org-mobile-inbox-for-pull "~/.org/from-mobile.org")

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Tasks") "* TODO %?\n  %u\n  %a")
        ("n" "Note" entry (file+headline "~/.org/notes.org" "Notes") "* %?\n  %u\n  %a")))

(define-key org-mode-map (kbd "C-'") nil)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(provide 'setup-org)
