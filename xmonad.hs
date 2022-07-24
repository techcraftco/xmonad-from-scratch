import System.Exit (exitSuccess)
import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)

import XMonad.Util.Run(spawnPipe)

main :: IO ()
main =
  do
    xmobar <- spawnPipe "xmobar"
    xmonad $
      docks $
        ewmh $
          def
            { modMask = mod4Mask,
              layoutHook = myLayout
            }
            `additionalKeysP` myKeys

myBrowser = "firefox"

myEmacs = "emacs"

myKeys =
  [ ("M-e e", spawn myEmacs),
    ("M-b", spawn myBrowser)
  ]

myLayout = avoidStruts $ Full ||| columns ||| Mirror columns ||| tcm
  where
    columns = Tall 1 (3 / 100) (1 / 2)
    tcm = ThreeColMid 1 (3 / 100) (1 / 2)

myLogHook xmobar = dynamicLogWithPP $ xmobarPP
