(define-key evil-normal-state-map "\C-t\C-g"
  (lambda () (interactive)
    (shell-command (concat "dow --gut counterpart -f " (buffer-filename)))))
