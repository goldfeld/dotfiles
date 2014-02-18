(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun grunt-find-root ()
  (let ((root (substring (shell-command-to-string "git rev-parse --show-toplevel") 0 -1)))
    (if (file-exists-p (concat root "/Gruntfile.js"))
      root
      (concat root "/src/main/webapp"))))

(defun grunt-cmd ()
  (concat "cd " (grunt-find-root) " && grunt --no-color"))

(defun grunt ()
  "Run grunt"
  (interactive)
  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
         (result (call-process-shell-command (grunt-cmd) nil grunt-buffer t))
         (output (with-current-buffer grunt-buffer (buffer-string))))
    (cond ((zerop result)
           (message "Grunt completed without errors"))
          (t
           (message nil)
           (split-window-vertically)
           (set-window-buffer (next-window) grunt-buffer)))))

(add-hook 'js2-mode-hook
          '(lambda ()
             (define-key evil-insert-state-map (kbd "C-l") '(lambda () (interactive)
                                                              (insert "console.log('');")
                                                              (backward-char 3)))
             (define-key evil-normal-state-map (kbd "C-m") 'grunt)))
