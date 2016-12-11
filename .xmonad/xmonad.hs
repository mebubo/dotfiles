import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Layout.Tabbed
import XMonad.Hooks.ManageDocks

tabConfig = defaultTheme {
    activeBorderColor = "#7cafc2",
    activeTextColor = "#ffffff",
    activeColor = "#7cafc2",
    inactiveBorderColor = "#585858",
    inactiveTextColor = "#ffffff",
    inactiveColor = "#585858",
    fontName = "xft:Hack:size=9"
}

baseConfig = desktopConfig

myLayout = avoidStruts $ tabbed shrinkText tabConfig ||| Tall 1 (3/100) (1/2) ||| Full

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ baseConfig
        { borderWidth = 1
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" ""
            }
        , terminal = "st"
        , modMask = mod4Mask
        , layoutHook = myLayout
        } `additionalKeys` [
           ((mod4Mask, xK_Escape), toggleWS)
        ]
