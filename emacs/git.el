(require 'find-on-github)

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
             (define-key git-rebase-mode-map "\C-p" 'git-rebase-move-line-up)
             (define-key git-rebase-mode-map "\C-n" 'git-rebase-move-line-down)
             (define-key magit-status-mode-map (kbd "q")
               'magit-quit-fullscreen-session)
             (define-key magit-status-mode-map (kbd "x")
               '(lambda (rev)
                  (interactive (list
                                (read-string "Reset head to (defaut HEAD^): "
                                             nil nil "HEAD^")))
                  (magit-reset-head rev)))))

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))
     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))
