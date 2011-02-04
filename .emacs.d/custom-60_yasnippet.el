(require 'yasnippet)
(require 'find-func)

(add-to-list 'yas/snippet-dirs (concat (file-name-directory (find-library-name "yasnippet"))
                                       "snippets"))
(let ((snipdir "~.emacs.d/snippets"))
  (when (file-directory-p snipdir)
    (add-to-list 'yas/snippet-dirs snipdir nil))) ; prepend to list

(yas/global-mode 1)
