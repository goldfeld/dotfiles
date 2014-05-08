(define-prefix-command 'dow-find-map)
(define-key evil-normal-state-map (kbd "C-t") 'dow-find-map)

(define-key dow-find-map "\C-t" 'ido-switch-buffer)
(define-key dow-find-map "\C-n" 'projectile-find-file)
(define-key dow-find-map "\C-c" 'projectile-switch-to-buffer)
(define-key dow-find-map "\C-d" 'find-file)

(define-key dow-find-map "s" 'eval-expression)
(define-key dow-find-map "\C-s" 'eval-last-sexp)
(define-key dow-find-map "f" 'eval-defun)
(define-key dow-find-map "\C-f" 'eval-buffer)

(define-key dow-find-map "\C-h" 'go-dow)
(define-key dow-find-map "\C-n" 'dow-find-file)
;(define-key clojure-mode-map (kbd "C-ctb") 'dow-browse)

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
