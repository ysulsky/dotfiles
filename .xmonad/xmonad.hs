import Data.List
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Config.Bluetile
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

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
  , ("M-S-<Return>", spawn $ terminal cfg)]
  `additionalKeys`
  [ ((0, xF86XK_AudioLowerVolume),
     spawn "pactl set-sink-volume @DEFAULT_SINK@ -- -1.5%")
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "pactl set-sink-volume @DEFAULT_SINK@ -- +1.5%")
  , ((0, xF86XK_AudioMute),
     spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")]

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
