;(add-hook 'clojure-mode-hook
;	  '(lambda ()
;	     (load-theme-buffer-local 'cyberpunk (current-buffer))))

(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl t)
(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
;; (add-hook 'nrepl-mode-hook 'smartparens-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;(define-key nrepl-mode-map "\C-t\C-s\C-l" 'nrepl-clear-buffer)
(add-hook 'nrepl-mode-hook
      '(lambda ()
	 (paredit-mode t)
	 ;(load-theme-buffer-local 'cyberpunk (current-buffer))
	 (define-key evil-insert-state-local-map (kbd "<up>") 'nrepl-backward-input)
	 (define-key evil-insert-state-local-map (kbd "<down>") 'nrepl-forward-input)
	 (define-key evil-insert-state-local-map (kbd "C-l") 'nrepl-clear-buffer)
	 (define-key evil-normal-state-local-map ",," 'nrepl-switch-to-last-clojure-buffer)))
