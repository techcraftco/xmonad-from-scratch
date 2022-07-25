import qualified Data.Map as M
import Data.Maybe (fromJust)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Hooks.ManageDocks

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    . withEasySB (statusBarProp "xmobar $HOME/.config/xmonad/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig

myBrowser = "firefox"

myEmacs = "emacs"

myWorkspaces = ["dev", "www", "comms", "sys"] ++ map show [5 .. 9]

myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

myConfig =
  def
    { modMask = mod4Mask,
      layoutHook = smartBorders myLayout,
      --startupHook = myStartupHook,
      workspaces = myWorkspaces
    }
    `additionalKeysP` [ ("M-S-b", spawn myBrowser),
                        ("M-e e", spawn myEmacs),
                        ("M-S-<Return>", spawn "kitty")
                      ]
myStartupHook = do
  spawnOnce "sleep 2; trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0xFF55FF --width 5 --height 30"

myLayout = tiled ||| Mirror tiled ||| Full ||| tcm
  where
    tiled = Tall 1 delta ratio
    tcm = ThreeColMid 1 delta ratio
    delta = (3 / 100)
    ratio = (1 / 2)

myXmobarPP :: PP
myXmobarPP =
  def
    { ppCurrent = xmobarColor draculaBackground draculaGreen . xmobarPadWS,
      ppVisible = xmobarColor draculaBackground draculaGreen . xmobarPadWS . xmobarClickable . xmobarPadWS,
      ppHidden = xmobarColor draculaForeground draculaComment . xmobarPadWS . xmobarClickable,
      ppHiddenNoWindows = xmobarColor draculaForeground draculaBackground . xmobarPadWS,
      ppTitleSanitize = xmobarStrip,
      ppSep = " | "
    }

xmobarPadWS = wrap " " " "

xmobarClickable ws = clickableWrap (i - 1) ws
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

draculaBackground = "#282a36"

draculaForeground = "#f8f8f8"

draculaGreen = "#50fa7b"

draculaComment = "#6272a4"
