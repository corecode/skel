(setq-default TeX-master 'dwim)
(setq-default TeX-auto-save t
              TeX-parse-self t
              reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (visual-line-mode t)
                             (flyspell-mode t)
                             (LaTeX-math-mode t)
                             (turn-on-reftex)
                             (TeX-PDF-mode t)))

(setq reftex-label-alist
      '(("compactenum" ?i "item:" nil item nil)))

(provide 'setup-tex)
