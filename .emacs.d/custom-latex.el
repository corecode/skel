(setq-default TeX-master 'dwim)
(setq TeX-auto-save t
      TeX-parse-self t
      reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (visual-line-mode t)
                             (flyspell-mode t)
                             (LaTeX-math-mode t)
                             (turn-on-reftex)
                             (TeX-PDF-mode t)))
