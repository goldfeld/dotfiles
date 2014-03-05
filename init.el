(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq require-final-newline nil)
(setq smooth-scroll-margin 5)

(setq eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias"))
(add-to-list 'load-path (expand-file-name "~/goldfeld/dotfiles/emacs/"))
(add-to-list 'load-path (expand-file-name "~/goldfeld/dotfiles/emacs/lib"))
(setq default-abbrev-mode t)
(setq-default indent-tabs-mode nil)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "luakit")

(load "packages.el")

(require 'evil)
(require 'evil-leader)
(require 'evil-matchit)
(global-evil-leader-mode)
(evil-mode t)
(global-evil-matchit-mode 1)
(evil-leader/set-leader ",")

(require 'projectile)
(require 'rainbow-delimiters)

(load "paredit.el")

(require 'column-enforce-mode)
(make-column-rule 80)
(add-hook 'text-mode-hook '80-column-rule)
(add-hook 'prog-mode-hook '80-column-rule)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'clojure-mode-hook 'turn-on-auto-fill)

(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'after-init-hook #'global-flycheck-mode)

;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(setq
 ac-auto-start nil
 ac-auto-show-menu 0.1
 ac-menu-height 20
 ac-modes (append ac-modes '(org-mode)))
(global-auto-complete-mode t)

;(load-theme 'solarized-dark t)
;(load-theme 'zenburn t)
(load-theme 'purple-haze t)

(require 'ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-flex-matching t)

;(require 'midnight)
;(midnight-delay-set 'midnight-delay "8:00am")
;(add-hook 'midnight-hook 'calendar)
; here's the place to create nightly build hooks

(require 'whitespace)
(setq whitespace-style '(face empty tabs))
;(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(setq indent-tabs-mode nil)

(defun string/starts-with (s arg)
  "returns non-nil if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
         (string-equal (substring s 0 (length arg)) arg))
        (t nil)))

(add-hook 'font-lock-mode (function (lambda ()
    (setq font-lock-keywords
          (append font-lock-keywords
                  '(("\t+" (0 'my-tab-face t))
                    ;("^.\\{81,\\}$" (0 'my-long-line-face t))
                    ("[ \t]+$"      (0 'my-trailing-space-face t))))))))

(load "evil-binds.el")
(load "cursor.el")
(load "clojure.el")
(load "js.el")
(load "projectile.el")
(load "prm.el")
(load "org-mode.el")
(load "git.el")
;(load "mawkro.el")

(defun load-custom-scratch ()
  "Load the contents of my custom scratch hints into the
  scratch buffer, clearing its contents first."
  (with-current-buffer (get-buffer "*scratch*")
    (delete-region (point-min) (point-max))
    (shell-command (format "cat %s"
     "~/goldfeld/dotfiles/emacs/scratch.el ~/.dow/today")
		   (current-buffer))))
(load-custom-scratch)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(my-tab-face ((((class color)) (:background "grey10"))) t)
 '(my-trailing-space-face ((((class color)) (:background "gray10"))) t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc3ba70e150efbe45db40b4b4886fc75716b4f3b1247a4b96e5be7cfbe4bc9e1" "968d1ad07c38d02d2e5debffc5638332696ac41af7974ade6f95841359ed73e3" "050beead9159996a613ba4bc734de8b13b882f1c6596d1dffa4f51d096662cf6" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "62b86b142b243071b5adb4d48a0ab89aefd3cf79ee3adc0bb297ea873b36d23f" "f5db04080a5133bc99721d680a11cf974d60d1df347b08841b43c3e97f52d3bf" "c5207e7b8cc960e08818b95c4b9a0c870d91db3eaf5959dd4eba09098b7f232b" default)))
 '(fringe-mode 6 nil (fringe))
 '(linum-format " %7d ")
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111"))
