(require 'erc)

(setq erc-nick "goldfeld")
(setq erc-user-full-name "Vic Goldfeld")
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(and
 (require 'erc-highlight-nicknames)
 (add-to-list 'erc-modules 'highlight-nicknames)
 (erc-update-modules))
