(c-add-style "2c"
             '("bsd"
               (c-hanging-braces-alist (brace-list-open)
                                       (brace-entry-open)
                                       (statement-cont)
                                       (substatement-open after)
                                       (block-close . c-snug-do-while)
                                       (extern-lang-open after)
                                       (namespace-open after)
                                       (module-open after)
                                       (composition-open after)
                                       (inexpr-class-open after)
                                       (inexpr-class-close before)
                                       (class-open after))
               (c-offsets-alist (label . [0]))))

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "2c")))

(define-key c-mode-map (kbd "<return>") 'c-context-line-break)

(provide 'setup-cc-mode)
