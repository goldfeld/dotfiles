(define-key evil-normal-state-map "\C-t\C-h" 'go-dow)

(defun go-dow ()
  (interactive)
  (shell-command "hooker 11"))
