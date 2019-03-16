(require 'znc)
(require 'erc)
(require 'erc-hl-nicks)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;; from <http://www.emacswiki.org/emacs/ErcFilling>
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

(defun 2c-erc-set-scroll-conservatively ()
  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 5))

(setq-default erc-ignore-list '("nox!" "evilbetty" "ohsix" "flyback" "mkad" "@ns.nurlon.be" "Johnsen" "dongs" "John___" "Mittens" "cutebutpsycho*" "moriarty" "jammi" "rpifan" "promach*"))

;; (setq-default erc-track-priority-faces-only 'all)
;; (setq-default erc-track-faces-priority-list
;;               (reduce
;;                (lambda (e l)
;;                  (remove l e))
;;                '(erc-default-face
;;                  (erc-button erc-default-face)
;;                  erc-action-face
;;                  (erc-nick-default-face erc-fool-face)
;;                  erc-fool-face)
;;                :initial-value erc-track-faces-priority-list))

(add-hook 'erc-mode-hook
          (lambda () (subword-mode 0)))

(provide 'setup-erc)
