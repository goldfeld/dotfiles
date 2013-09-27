(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)

;; loads the package manager
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(defvar my-packages '(evil
                      evil-leader
                      projectile
                      helm
                      helm-projectile
                      magit
                      paredit
                      rainbow-delimiters
                      color-theme
                      clojure-mode
                      clojure-test-mode
                      nrepl
                      solarized-theme
                      zenburn-theme
                      soothe-theme
                      purple-haze-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'helm-config)
(require 'rainbow-delimiters)
(require 'paredit)
(require 'projectile)
	 
(projectile-global-mode)

;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-arjen)
(load-theme 'solarized-dark t)

(require 'evil)
(evil-mode t)

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-flex-matching t)

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
;; unbind <CR> and <Space> in evil so other modes can use them.
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

; (define-key evil-normal-state-map "c" nil)
; (define-key evil-motion-state-map "cu" 'universal-argument)

; (define-key key-translation-map (kbd "ch") (kbd "C-h"))
; (define-key key-translation-map (kbd "cx") (kbd "C-x"))

; https://github.com/emacsmirror/evil/blob/master/evil-maps.el

(define-key evil-insert-state-map "\C-h" (kbd "<backspace>"))
(define-key evil-ex-completion-map "\C-h" (kbd "<backspace>"))

(define-key evil-normal-state-map "\C-t" nil)
(define-key evil-normal-state-map "\C-t\C-h" 'ido-switch-buffer)

(define-key evil-motion-state-map "," nil)
(define-key evil-normal-state-map ",.v" (kbd ":e /home/vic/goldfeld/dotfiles/init.el"))

(define-key evil-normal-state-map "m" nil)
(define-key evil-motion-state-map "mm" 'evil-ex)
(define-key evil-normal-state-map "mw" (kbd ":w"))
(define-key evil-normal-state-map "mq" (kbd ":q"))
(define-key evil-normal-state-map "mv" (kbd ":vsplit"))
(define-key evil-normal-state-map "mz" (kbd ":split"))
(define-key evil-normal-state-map "mo" (kbd "O"))
(define-key evil-normal-state-map "mj" (kbd ":m+"))
(define-key evil-normal-state-map "mk" (kbd "ddkP"))

(define-key evil-normal-state-map "gs" 'magit-status)

;(dolist (p '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "n" "p" "u" "x" "y"))
;  (define-key evil-normal-state-map (vconcat "m" p) (lambda () (evil-set-marker p 0))
;(define-key evil-motion-state-map (vconcat "z" [return]) "zt^")
;(define-key evil-motion-state-map (kbd "z RET") (vconcat "z" [return]))

;(define-key evil-normal-state-map "t" nil)
(define-key evil-normal-state-map "t\C-m" (lambda () (interactive) (shell-command "hooker 3")))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c5207e7b8cc960e08818b95c4b9a0c870d91db3eaf5959dd4eba09098b7f232b" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
