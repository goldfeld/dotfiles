(require 'evil-leader)
(require 'evil-matchit)
(global-evil-leader-mode)
(evil-mode t)
(evil-leader/set-leader ",")

(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evilmi-jump-items
    ",%i" 'evilmi-select-items
    ",%i" 'evilmi-delete-items))
(global-evil-matchit-mode 1)

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
;; unbind <CR> and <Space> in evil so other modes can use them.
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

(define-prefix-command 'workflow-map)
(global-set-key "\C-cw" 'workflow-map)
(define-key workflow-map (kbd "c") 'org-wc-display)
(define-key workflow-map (kbd "p") 'pcomplete)

; (define-key evil-normal-state-map "c" nil)
; (define-key evil-motion-state-map "cu" 'universal-argument)

; (define-key key-translation-map (kbd "ch") (kbd "C-h"))
; (define-key key-translation-map (kbd "cx") (kbd "C-x"))

; https://github.com/emacsmirror/evil/blob/master/evil-maps.el

(define-key evil-normal-state-map (kbd "C-+") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "C--") 'evil-window-decrease-height)

(define-key evil-insert-state-map (kbd "C--")
  (lambda () (interactive) (set-face-attribute 'default nil :height 130)))
(define-key evil-insert-state-map (kbd "C-+")
  (lambda () (interactive) (set-face-attribute 'default nil :height 165)))

(define-key evil-insert-state-map (kbd "C-SPC") 'dabbrev-expand)
(define-key evil-insert-state-map (kbd "C-p") 'complete-symbol)
(define-key evil-insert-state-map (kbd "C-&") (lambda () (interactive)
                                                (evil-backward-char)
                                                (evil-jump-item)))


(defun backward-kill-line (arg) (interactive "p") (kill-line (- 1 arg)))

(define-key evil-insert-state-map "\C-h" (kbd "<backspace>"))
(define-key evil-ex-completion-map "\C-h" (kbd "<backspace>"))
(define-key evil-insert-state-map "\C-u" 'backward-kill-line)
(define-key evil-ex-completion-map "\C-u" 'backward-kill-line)

(define-key evil-ex-completion-map (kbd "C-SPC") 'evil-ex-completion)

; unmap to avoid conflict with my org-mode custom link navigation maps
(define-key evil-normal-state-map "\C-n" nil)
(define-key evil-normal-state-map "\C-p" nil)

; output current time and date with year and week, all pretty printed.
(defun date-and-battery () (interactive)
  (let ((date (shell-command-to-string "date +'[%Yw%V] %b %-e %a <%H:%M>'"))
        (battery (shell-command-to-string "acpi")))
    (message "%s" (concat date " " battery))))

(evil-leader/set-key
 "p" (lambda () (interactive) (kbd "\C-u\M-x org-insert-drawer RET"))
 "," 'switch-to-alt-buffer
 "d" 'date-and-battery
 ".v" (kbd ":e ~/goldfeld/dotfiles/init.el")
 ".t" (kbd ":e ~/dow/life.tnt.org")
 ".x" (shell-command "xcape -e 'Control_L=Escape'")
 "c" 'flycheck-next-error
 "r" 'flycheck-previous-error
 "g" 'flycheck-first-error
 "l" 'flycheck-list-errors)

(evil-ex-define-cmd "eval" 'eval-expression)

;(define-key evil-normal-state-map "gs" 'dow-status)
(define-key evil-normal-state-map "gs" 'magit-status)
(define-key evil-normal-state-map "g#" 'projectile-grep)
(define-key evil-visual-state-map "g#" 'projectile-grep)
(define-key evil-normal-state-map "gb" 'vc-annotate)
(define-key evil-normal-state-map "gB" 'browse-on-github)
(define-key evil-normal-state-map "gt"
  (lambda () (interactive)
    (shell-command (concat "git add " (buffer-file-name)))))

;(dolist (p '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "n" "p" "u" "x" "y"))
;  (define-key evil-normal-state-map (vconcat "m" p) (lambda () (evil-set-marker p 0))
;(define-key evil-motion-state-map (vconcat "z" [return]) "zt^")
;(define-key evil-motion-state-map (kbd "z RET") (vconcat "z" [return]))

;(define-key evil-normal-state-map "t" nil)
(define-key evil-normal-state-map "t\C-m" (lambda () (interactive)
                                            (shell-command "hooker 3")))

(define-key evil-normal-state-map "\C-c\C-rc"
  (lambda () (interactive)
    (shell-command "cd ~/live && git checkout src/main/webapp/public/assets")))