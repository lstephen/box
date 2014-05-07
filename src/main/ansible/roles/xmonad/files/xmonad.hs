import XMonad
import XMonad.Hooks.DynamicLog

import Data.Monoid

main = xmonad =<< statusBar "xmobar" myPP myToggleKey myConfig

myPP = defaultPP
  { ppTitle = const ""
  , ppCurrent = orange . append "*"
  , ppVisible = base0 . append " "
  , ppHidden = base0 . append "-"
  , ppHiddenNoWindows = base0 . append " "
  , ppSep = base01 " : "
  , ppLayout = green
  , ppUrgent = red
  }

myConfig = defaultConfig
  { focusedBorderColor = redHex
  , terminal = "rxvt-unicode"
  }

myToggleKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

append = flip mappend

base0 = xmobarColor "#808080" "" 
base01 = xmobarColor "#4e4e4e" ""
orange = xmobarColor "#d75f00" ""
green = xmobarColor "#5f8700" ""
red = xmobarColor redHex ""

redHex = "#af0000"

