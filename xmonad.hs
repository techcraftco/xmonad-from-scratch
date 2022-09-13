import qualified Data.Map as M
import Data.Maybe (fromJust)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar $HOME/.config/xmonad/xmobarrc" (pure (filterOutWsPP [scratchpadWorkspaceTag] myXmobarPP))) defToggleStrutsKey
    $ myConfig

myBrowser = "firefox"

myEmacs = "emacs"

myWorkspaces = ["dev", "www", "comms", "sys"] ++ map show [5 .. 9]

myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

myConfig =
  def
    { modMask = mod1Mask,
      layoutHook = smartBorders myLayout,
      manageHook = myManageHook <+> namedScratchpadManageHook scratchpads,
      logHook = refocusLastLogHook >> nsHideOnFocusLoss hideOnFocusLossSP,
      workspaces = myWorkspaces,
      focusedBorderColor = draculaGreen,
      normalBorderColor = draculaComment
    }
    `additionalKeysP` [ ("M-S-b", spawn myBrowser),
                        ("M-e e", spawn myEmacs),
                        ("M-S-<Return>", spawn "kitty"),
                        ("M-S-h", namedScratchpadAction scratchpads "htop"),
                        ("M-S-n", namedScratchpadAction scratchpads "org"),
                        ("M-S-m", namedScratchpadAction scratchpads "fastmail")
                      ]

myLayout = tiled ||| Mirror tiled ||| Full ||| tcm
  where
    tiled = Tall 1 delta ratio
    tcm = ThreeColMid 1 delta ratio
    delta = (3 / 100)
    ratio = (1 / 2)

myManageHook =
  composeAll
    [ className =? "firefox" --> doShift (myWorkspaces !! 1),
      className =? "Gimp" --> doFloat
    ]

scratchpads =
  [ NS "htop" "kitty --class htop  htop" (className =? "htop") defaultFloating,
    NS "org" "emacsclient --eval '(+org-capture/open-frame \"\" \"n\")'" (title =? "doom-capture") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
    NS "fastmail" "firefox --class fastmail -P fastmail fastmail.com" (className =? "fastmail") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]

hideOnFocusLossSP =
  filter (\ns -> elem (name ns) hide) scratchpads
  where
    hide = ["htop"]

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
