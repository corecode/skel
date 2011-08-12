(setq-default fixme-password "")

(condition-case ex
    (load (concat dotfiles-dir "passwords-nocommit.el"))
  ('error))
