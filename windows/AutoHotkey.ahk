SetTitleMatchMode, 2

; assign a hotkey to launch a app
; #n::Run Notepad     ; this means the Win+n
; !n::Run Notepad     ; this means Alt+n
; ^n::Run Notepad     ; this means Ctrl+n

~LWin Up::Return
; ~RWin Up::Return

Lwin::return
; Rwin::return

$AppsKey::Send !{Tab 2}

LWin & Esc::Send !{Tab}

Lwin & 1::
IfWinActive, Google Chrome
{
    WinGetClass, CurrentActive, A
    WinGet, Instances, Count, ahk_class %CurrentActive%
    If Instances > 1
        WinActivateBottom, ahk_class %CurrentActive%
    Return
} else
{
    IfWinExist Google Chrome
        WinActivate Google Chrome,, Trace Panel
    Return
}

LWin & 2::
IfWinExist IntelliJ
    WinActivate
Return

LWin & 3::
IfWinExist tmux: 0
    WinActivate
Return

LWin & 4::
IfWinExist Terminal Emulator 2
    WinActivate
Return

LWin & 5::
IfWinExist emacs:
    WinActivate
Return

LWin & 6::
IfWinExist Eclipse
    WinActivate
Return

LWin & 7::
IfWinExist Sublime Text
    WinActivate
Return

LWin & 9::
IfWinExist IN@
    WinActivate
Return

LWin & 0::
IfWinExist Outlook
    WinActivate
Return

+#Up::
    WinGetActiveTitle, Title
    WinRestore, %Title%
    SysGet, X1, 76
    SysGet, Y1, 77
    WinMove, %Title%,, X1, Y1, 3360, 1010
return

; LWin & Up::WinMaximize, A

LWin & Del::SoundSet -1
LWin & End::Send {Volume_Mute}
LWin & PgDn::SoundSet +1

LWin & c::
RWin & c::
Input Key, L1
if Key=j
{
    IfWinExist, Conversations
        WinActivate
    else
        WinActivate, Jabber
}
if Key=s
{
    IfWinExist, S [started
        WinActivate
    else
        WinActivate, Sametime Connect
}
if Key=o
{
    IfWinExist, Outlook
        WinActivate
}
return

LWin & d::
RWin & d::
Input Key, L1
if Key=i
{
    IfWinExist, IntelliJ
        WinActivate
}
if Key=e
{
    IfWinExist, emacs
        WinActivate
}
if Key=s
{
    IfWinExist, Sublime Text
        WinActivate
}
if Key=t
{
    IfWinExist, tmux:
        WinActivate
}
return
