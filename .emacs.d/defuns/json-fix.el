(require 'json)


(defun json-alist-p (list)
  "Non-null if and only if LIST is an alist with simple keys."
  (while (consp list)
    (setq list (if (and (consp (car list))
			(atom (caar list)))
                   (cdr list)
                 'not-alist)))
  (null list))
