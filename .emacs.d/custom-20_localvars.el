(mapc (lambda (pair)
        (put (car pair) 'safe-local-variable (cdr pair)))
      '((espresso-indent-level . integerp)))
