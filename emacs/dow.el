(define-key evil-normal-state-map "\C-t" nil)
(define-key evil-normal-state-map "\C-t\C-t" 'ido-switch-buffer)
(define-key evil-normal-state-map "\C-t\C-n" 'projectile-find-file)
(define-key evil-normal-state-map "\C-t\C-c" 'projectile-switch-to-buffer)
(define-key evil-normal-state-map "\C-t\C-d" 'find-file)

(define-key evil-normal-state-map "\C-ts" 'eval-expression)
(define-key evil-normal-state-map "\C-t\C-s" 'eval-last-sexp)
(define-key evil-normal-state-map "\C-tf" 'eval-defun)
(define-key evil-normal-state-map "\C-t\C-f" 'eval-buffer)

(define-key evil-normal-state-map "\C-t\C-h" 'go-dow)
(define-key evil-normal-state-map "\C-t\C-n" 'dow-find-file)
(define-key clojure-mode-map (kbd "C-ctb") 'dow-browse)

(defun go-dow ()
  (interactive)
  (shell-command "hooker 11"))

(defun dow-browse (prj-dir)
  (interactive "D")
  (term "/bin/bash")
  (term-send-raw-string (concat "dow browse -p " dir))
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(defun dow-find-file ()
  (interactive)
  (let ((pick (shell-command (concat "dow find " (buffer-file-name)))))
    (message "%s" (shell-command "git ls-files"))
    (find-file pick)))
