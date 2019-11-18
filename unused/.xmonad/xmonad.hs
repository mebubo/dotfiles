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
import qualified XMonad.StackSet as W

tabConfig = def {
    activeBorderColor = "#7cafc2",
    activeTextColor = "#ffffff",
    activeColor = "#7cafc2",
    inactiveBorderColor = "#585858",
    inactiveTextColor = "#ffffff",
    inactiveColor = "#585858",
    fontName = "xft:monospace:size=9"
}

baseConfig = desktopConfig

myLayout = avoidStruts $ lessBorders OnlyFloat $ smartBorders (Tall 1 (3/100) (1/2)) ||| noBorders Full

sink = "$(pactl list short sinks | (grep RUNNING || echo 'alsa_output.pci-0000_00_1b.0.analog-stereo') | cut -f1)"

myManageHook = composeAll
    [ className =? "Pavucontrol" --> doFloat
    , className =? "mpv" --> doFloat
    ]

myTerminal = "st"
myModMask = mod4Mask

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ baseConfig
        { borderWidth = 1
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" ""
            }
        , terminal = myTerminal
        , modMask = myModMask
        , layoutHook = myLayout
        , handleEventHook = fullscreenEventHook
        , manageHook = myManageHook <+> manageHook baseConfig
        } `additionalKeysP`
           [ ("M4-<Esc>", toggleWS)
           , ("<XF86AudioRaiseVolume>", spawn $ "pactl set-sink-volume " ++ sink ++ " +2%")
           , ("<XF86AudioLowerVolume>", spawn $ "pactl set-sink-volume " ++ sink ++ " -2%")
           , ("<XF86AudioMute>", spawn $ "pactl set-sink-mute " ++ sink ++ " toggle")
           , ("<XF86Display>", spawn "external-display.sh auto")
           , ("M4-<F2>", spawn "i3lock -c 330033 -d")
           , ("M4-d", spawn "dmenu_run")
           , ("M4-p", spawn "pavucontrol")
           , ("M4-<Return>", spawn myTerminal)
           , ("M4-S-<Return>", windows W.swapMaster)
           ] `removeKeys` [
             (myModMask .|. shiftMask, xK_q)
           ]
