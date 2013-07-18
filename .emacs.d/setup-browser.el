;;; from <http://www.emacswiki.org/cgi-bin/wiki/JorgenSchaefersEmacsConfig>
(defun 2c-choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use external browser? ")
      (browse-url-chromium url)
    (w3m-browse-url url)))

(setq browse-url-browser-function '2c-choose-browser)

(provide 'setup-browser)
