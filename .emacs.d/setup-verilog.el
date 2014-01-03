(require 'verilog-mode)

(eval-after-load 'iedit
  '(define-key verilog-mode-map (kbd "C-;") iedit-mode))

(provide 'setup-verilog)
