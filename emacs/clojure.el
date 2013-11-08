(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl t)
(add-to-list 'same-window-buffer-names "*nrepl*")

(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (paredit-mode t)))
;	     (load-theme-buffer-local 'cyberpunk (current-buffer))))

(require 'ac-nrepl)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(add-hook 'nrepl-mode-hook 'subword-mode)
;(add-hook 'nrepl-mode-hook 'paredit-mode)
;(add-hook 'nrepl-mode-hook 'smartparens-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;(define-key nrepl-mode-map "\C-t\C-s\C-l" 'nrepl-clear-buffer)
(add-hook 'nrepl-mode-hook
      '(lambda ()
	 (paredit-mode t)
	 ;(load-theme-buffer-local 'cyberpunk (current-buffer))
	 (define-key evil-insert-state-local-map (kbd "<up>") 'nrepl-backward-input)
	 (define-key evil-insert-state-local-map (kbd "<down>") 'nrepl-forward-input)
	 (define-key evil-insert-state-local-map (kbd "C-l") 'nrepl-clear-buffer)
	 (define-key evil-normal-state-local-map ",," 'nrepl-switch-to-last-clojure-buffer)))
