(require 'erc)

(setq erc-nick "goldfeld")
(setq erc-user-full-name "Vic Goldfeld")
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(and
 (require 'erc-hl-nicks)
 (add-to-list 'erc-modules 'hl-nicks)
 (erc-update-modules))
