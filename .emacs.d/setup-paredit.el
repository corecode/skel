;; My keybindings for paredit

(require 'paredit)

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(diminish 'paredit-mode "()")

(provide 'setup-paredit)
