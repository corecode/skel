(require 'typescript-mode)
(require 'company)
(require 'tide)

(diminish 'company-mode)
(diminish 'tide-mode)
(diminish 'flycheck-mode)

;; sample config
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(mode-enabled idle-change new-line mode-enabled))
            (eldoc-mode t)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Use ƒ for anonymous functions
(font-lock-add-keywords
 'typescript-mode `(("\\_<\\(function\\) *("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil)))))

(provide 'setup-ts-mode)
