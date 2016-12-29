import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Layout.Tabbed
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Layout.NoBorders

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

myLayout = lessBorders OnlyFloat $ avoidStruts $ smartBorders (Tall 1 (3/100) (1/2)) ||| noBorders Full ||| (tabbed shrinkText tabConfig)

sink = "$(pactl list short sinks | (grep RUNNING || echo 'alsa_output.pci-0000_00_1b.0.analog-stereo') | cut -f1)"

myManageHook = composeAll
    [ className =? "Pavucontrol" --> doFloat
    , className =? "mpv" --> doFloat
    ]

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
        , handleEventHook = fullscreenEventHook
        , manageHook = myManageHook <+> manageHook baseConfig
        } `additionalKeysP`
           [ ("M4-<Esc>", toggleWS)
           , ("<XF86AudioRaiseVolume>", spawn $ "pactl set-sink-volume " ++ sink ++ " +2%")
           , ("<XF86AudioLowerVolume>", spawn $ "pactl set-sink-volume " ++ sink ++ " -2%")
           , ("<XF86AudioMute>", spawn $ "pactl set-sink-mute " ++ sink ++ " toggle")
           , ("<XF86Display>", spawn $ "external-display.sh")
           , ("M4-<F2>", spawn $ "i3lock -c 330033 -d")
           , ("M4-d", spawn $ "rofi -show run -columns 3")
           , ("M4-p", spawn $ "pavucontrol")
        ]
