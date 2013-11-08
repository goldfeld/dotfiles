(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias"))
(add-to-list 'load-path (expand-file-name "~/goldfeld/dotfiles/emacs/"))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))

(load "packages.el")

(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-mode t)
(evil-leader/set-leader ",")

(require 'helm-config)
(require 'projectile)
(require 'rainbow-delimiters)

(load "paredit.el")

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

(require 'midnight)
(midnight-delay-set 'midnight-delay "8:00am")
(add-hook 'midnight-hook 'calendar)
; here's the place to create nightly build hooks

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
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
		    ("^.\\{81,\\}$" (0 'my-long-line-face t))
		    ("[ \t]+$"      (0 'my-trailing-space-face t))))))))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(load "evil-binds.el")
(load "cursor.el")
(load "clojure.el")
(load "projectile.el")
(load "org-mode.el")
;(load "mawkro.el")

(require 'org-collector)

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
 '(custom-safe-themes (quote ("050beead9159996a613ba4bc734de8b13b882f1c6596d1dffa4f51d096662cf6" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "62b86b142b243071b5adb4d48a0ab89aefd3cf79ee3adc0bb297ea873b36d23f" "f5db04080a5133bc99721d680a11cf974d60d1df347b08841b43c3e97f52d3bf" "c5207e7b8cc960e08818b95c4b9a0c870d91db3eaf5959dd4eba09098b7f232b" default))))
