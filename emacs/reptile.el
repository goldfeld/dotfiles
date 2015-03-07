(define-prefix-command 'repfiles-map)
(define-key evil-normal-state-map (kbd "C-l") 'repfiles-map)

(define-key repfiles-map "\C-c"
  '(lambda () (interactive)
     ;; need to be able to supply search argument, and to split the result which
     ;; which will be in the format "[context_path]\t[line_number]"
     (find-file (substring (shell-command-to-string "rep file -P") 0 -1))))

(define-key repfiles-map "\C-n"
  '(lambda () (interactive)
     (find-file (substring (shell-command-to-string "rep file -a") 0 -1))))

(define-key repfiles-map "\C-l"
  '(lambda () (interactive)
     (outline-isearch-open-invisible "setq")
     ))

(define-prefix-command 'reptile-session-map)
(define-key evil-normal-state-map (kbd "C-t") 'reptile-session-map)

(define-key reptile-session-map "\C-t" 'ido-switch-buffer)
(define-key reptile-session-map "\C-d" 'find-file)
(define-key reptile-session-map "\C-n"
  '(lambda () (interactive)
     (find-file (substring (shell-command-to-string "rep session -a --source git") 0 -1))))

(define-key reptile-session-map "\C-c"
  '(lambda () (interactive)
     (find-file (substring (shell-command-to-string "rep session -e --source git") 0 -1))))

(define-key reptile-session-map "s" 'eval-expression)
(define-key reptile-session-map "\C-s" 'eval-last-sexp)
(define-key reptile-session-map "f" 'eval-defun)
(define-key reptile-session-map "\C-f" 'eval-buffer)

(define-key reptile-session-map "\C-h" 'go-dow)
(define-key reptile-session-map "\C-u" 'dow-flow-ignore-next-action)
(define-key reptile-session-map "\C-n" 'dow-find-file)
;(define-key clojure-mode-map (kbd "C-ctb") 'dow-browse)

(define-prefix-command 'dow-find-presets-map)
(define-key reptile-session-map (kbd "C-l") 'dow-find-presets-map)

(defun go-dow ()
  (interactive)
  (shell-command "hooker 11"))

(defun dow-flow-ignore-next-action ()
  (interactive)
    (shell-command "dow flow --ignore-next-action"))

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
