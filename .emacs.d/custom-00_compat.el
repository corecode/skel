(unless (boundp 'orig-called-interactively-p)
  (defvar orig-called-interactively-p (symbol-function 'called-interactively-p)))

(defun called-interactively-p (&optional arg)
  (condition-case nil
      (funcall orig-called-interactively-p arg)
    (error
     (funcall orig-called-interactively-p))))
