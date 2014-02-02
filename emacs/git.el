(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-fullscreen-session ()
  "Restores the previous window configuration and kills magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(add-hook 'magit-status-mode-hook
          '(lambda ()
             (define-key magit-status-mode-map (kbd "q") 'magit-quit-fullscreen-session)))
