import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.Commands
import XMonad.Actions.UpdatePointer
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import System.IO

main = xmonad $ defaultConfig
     { handleEventHook = serverModeEventHook
     , focusFollowsMouse = False
     , terminal = "terminator"
     } `additionalKeysP` [ ("M-S-u", spawn "terminator")
                         , ("M-u", windows W.swapMaster)
                         ]