import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ defaultConfig 
     { modMask    = mod4Mask
     , manageHook = manageDocks <+> manageHook defaultConfig
     , handleEventHook = fullscreenEventHook
     , layoutHook = avoidStruts  $  layoutHook defaultConfig
     , logHook    = dynamicLogWithPP xmobarPP
                      { ppOutput = hPutStrLn xmproc
                      , ppOrder  = (\(ws:lo:_) -> [ws, lo]) }
     , workspaces = map show $ [ 1 .. 9 ] ++ [ 0 :: Int ]
     } `removeKeysP`
     [ "M-q", "M-S-q"
     ] `additionalKeysP` myKeys
     
myKeys = 
  [ ("M-S-r", spawn "dmenu_run -b -i -nb black -nf white -sb gray -sf red -fn -*-terminus-bold-*-*-*-*-120-*-*-*-*-*-*")
  , ("M-S-l", spawn "xscreensaver-command -lock")
  , ("M-0",   windows $ W.greedyView "0")
  , ("M-S-0",   windows $ W.shift "0")
  ]
