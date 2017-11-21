import Data.List
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Config.Bluetile
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

-- import XMonad.Hooks.EwmhDesktops
-- import XMonad.Hooks.ManageDocks
-- import qualified XMonad.StackSet as W
-- import XMonad.Util.Run(spawnPipe)
-- import System.Directory

-- main = do
--   xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
--   xmonad $ def
--      { modMask    = mod4Mask
--      , manageHook = manageDocks <+> manageHook defaultConfig
--      , handleEventHook = fullscreenEventHook
--      , layoutHook = avoidStruts  $  layoutHook defaultConfig
--      , logHook    = dynamicLogWithPP xmobarPP
--                       { ppOutput = hPutStrLn xmproc
--                       , ppOrder  = (\(ws:lo:_) -> [ws, lo]) }
--      , workspaces = map show $ [ 1 .. 9 ] ++ [ 0 :: Int ]
--      , startupHook= spawn "S=~/.xmonad/session; [ -x $S ] && $S"
--      } `removeKeysP`
--      [ "M-q", "M-S-q"
--      ] `additionalKeysP` myKeys

-- Color of current window title in xmobar.
xmobarTitleColor :: String
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "#CEFFAC"

runSession :: String
runSession = "S=~/.xmonad/session; [ -x $S ] && $S"

screensaver :: String
screensaver = "gnome-screensaver-command --lock"

dmenu :: String
dmenu = intercalate " "
  [ "dmenu_run"
  , "-b -i -nb black -nf white -sb gray -sf red"
  , "-fn -*-terminus-bold-*-*-*-*-120-*-*-*-*-*-*"
  ]

setKeys :: XConfig a -> XConfig a
setKeys cfg = cfg
  `removeKeysP`
   [ "M-o", "M-q", "M-S-q" ]
  `additionalKeysP`
   [ ("M-S-r", spawn dmenu)
   , ("M-S-l", spawn screensaver)
   , ("M-<Return>", spawn $ terminal cfg)
   , ("M-S-<Return>", spawn $ terminal cfg)
  ]
  `additionalKeys`
  [
    ((0, xF86XK_AudioLowerVolume   ), spawn "pactl sink-set-volume 0 -- -1.5%")
  , ((0, xF86XK_AudioRaiseVolume   ), spawn "pactl sink-set-volume 0 -- +1.5%")
  , ((0, xF86XK_AudioMute          ), spawn "pactl sink-mute 0 toggle")
  ]

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  let cfg = bluetileConfig {
        modMask = mod4Mask
        , startupHook = spawn runSession
        , logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
            , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
            , ppSep = "   "
            }
        }
  xmonad $ setKeys cfg
