import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
import XMonad.Actions.Commands
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = xmonad defaultConfig
     { handleEventHook = serverModeEventHook
     , terminal = "terminator"
     }
