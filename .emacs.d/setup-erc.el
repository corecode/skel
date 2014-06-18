(require 'erc)
(require 'erc-hl-nicks)
(require 'znc)

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

(define-erc-module scroll-conservatively nil
  "Make the prompt stay at the bottom of the window."
  ((add-hook 'erc-mode-hook '2c-erc-set-scroll-conservatively)
   (mapcar (lambda (buf)
             (with-current-buffer buf
               (2c-erc-set-scroll-conservatively)))
           (erc-buffer-list)))
  (t))

(defun 2c-erc-set-scroll-conservatively ()
  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 5))

(setq-default erc-ignore-list '("ttmrichter!" "nox!" "evilbetty"))

(provide 'setup-erc)
