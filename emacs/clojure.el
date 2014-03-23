(setq nrepl-hide-special-buffers t
      cider-repl-pop-to-buffer-on-connect nil
      cider-popup-stacktraces nil
      cider-repl-popup-stacktraces t)

(add-to-list 'same-window-buffer-names "*cider*")

(define-global-abbrev "defnn" "defn ^:private")

(add-hook 'clojure-mode-hook
          '(lambda ()
             (setq projectile-globally-ignored-directories
                   (append '("target" "crossover-cljs")
                           projectile-globally-ignored-directories))
             (paredit-mode t)))
;	     (load-theme-buffer-local 'cyberpunk (current-buffer))))

;(require 'ac-nrepl)

(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'cider-mode))
;(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)

;(define-key cider-mode-map "\C-t\C-s\C-l" 'cider-clear-buffer)
(add-hook 'cider-repl-mode-hook
      '(lambda ()
         ;(load-theme-buffer-local 'cyberpunk (current-buffer))
         (define-key evil-insert-state-local-map (kbd "<up>") 'cider-repl-backward-input)
         (define-key evil-insert-state-local-map (kbd "<down>") 'cider-repl-forward-input)
         (define-key evil-insert-state-local-map (kbd "C-l") 'cider-repl-clear-buffer)
         (define-key evil-insert-state-local-map (kbd "C-l") 'cider-repl-clear-buffer)
         (define-key evil-insert-state-local-map (kbd "C-l") 'cider-repl-kill-input)))
