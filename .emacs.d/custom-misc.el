;; make raise-frame work correctly
;; from: <http://article.gmane.org/gmane.emacs.devel:39702>
(defadvice raise-frame (after make-it-work (&optional frame) activate)
  "Make it work."
  (call-process
   "wmctrl" nil nil nil "-i" "-R"
   (frame-parameter (or frame (selected-frame)) 'outer-window-id)))
