(define-erc-module scroll-conservatively nil
  "Make the prompt stay at the bottom of the window."
  ((add-hook 'erc-mode-hook '2c-erc-set-scroll-conservatively)
   (mapcar (lambda (buf)
             (with-current-buffer buf
               (2c-erc-set-scroll-conservatively)))
           (erc-buffer-list)))
  (t))

(provide 'erc-scroll-conservatively)
