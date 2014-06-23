import Data.Monoid

import XMonad hiding (Color)
import XMonad.Hooks.DynamicLog

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig

main :: IO ()
main = statusBar "xmobar" myPP myToggleKey myConfig >>= xmonad

myPP :: PP
myPP = defaultPP
  { ppTitle = const ""
  , ppCurrent = xmbc Orange
  , ppVisible = xmbc Base01
  , ppHidden = xmbc Base1
  , ppHiddenNoWindows = xmbc Base01
  , ppSep = (xmbc Base01) " : "
  , ppLayout = xmbc Green
  , ppUrgent = xmbc Red
  }

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig = defaultConfig
  { focusedBorderColor = hex Red
  , normalBorderColor = hex Base01
  , terminal = "urxvtc"
  } `additionalKeysP` myKeys

myToggleKey :: XConfig t -> (KeyMask, KeySym)
myToggleKey XConfig { modMask = mm } = (mm, xK_b)

myKeys :: [ (String, X()) ]
myKeys =
  [ ("M-p", shellPrompt myXPConfig)
  --, ((0, xK_Super_L), return ()) -- have XMonad swallow Windows key
  -- The above line can't be added in here. Work out where later
  ]

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { position = Top
  , font = "xft:Ubuntu Mono:size=12"
  , bgColor = hex Base02
  , fgColor = hex Base1
  , bgHLight = hex Base02
  , fgHLight = hex Orange
  , promptBorderWidth = 1
  , borderColor = hex Base01
  , alwaysHighlight = True
  , height = 20
  }

append :: (Monoid a) => a -> a -> a
append = flip mappend

data Color = Base03 | Base02 | Base01 | Base0 | Base1 | Red | Green | Orange

hex :: Color -> String
hex Base03 = "#002b36"
hex Base02 = "#073642"
hex Base01 = "#586e75"
hex Base0  = "#839496"
hex Base1  = "#93a1a1"
hex Red    = "#dc322f"
hex Green  = "#869900"
hex Orange = "#cb4b16"

xmbc :: Color -> WorkspaceId -> String
xmbc c = xmobarColor (hex c) ""

