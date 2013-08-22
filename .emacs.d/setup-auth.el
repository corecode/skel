(require 'auth-source)
(require 'secrets)

(setq auth-sources '("~/.authinfo.gpg"))

(condition-case ex
    (load (concat dotfiles-dir "passwords-nocommit.el"))
  ('error))

(provide 'setup-auth)
