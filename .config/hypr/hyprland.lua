------------------
---- MONITORS ----
------------------

hl.monitor({ output = "eDP-1", mode = "preferred", position = "0x0", scale = 2 })
-- hl.monitor({ output = "eDP-1", mode = "preferred", position = "0x0", scale = 1.566667 })
-- hl.monitor({ output = "",     mode = "preferred", position = "auto", scale = "auto" })


---------------------
---- MY PROGRAMS ----
---------------------

local terminal    = "ghostty"
local fileManager = "dolphin"
local menu        = "rofi -show drun -show-icons"
local menu2       = "rofi -show run"


-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")


-----------------------
----- PERMISSIONS -----
-----------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Permissions/
-- Please note permission changes here require a Hyprland restart and are not applied on-the-fly
-- for security reasons

-- hl.config({
--   ecosystem = {
--     enforce_permissions = true,
--   },
-- })

-- hl.permission("/usr/(bin|local/bin)/grim", "screencopy", "allow")
-- hl.permission("/usr/(lib|libexec|lib64)/xdg-desktop-portal-hyprland", "screencopy", "allow")
-- hl.permission("/usr/(bin|local/bin)/hyprpm", "plugin", "allow")


-----------------------
---- LOOK AND FEEL ----
-----------------------

hl.config({
    general = {
        gaps_in  = 0,
        gaps_out = 0,

        border_size = 1,

        col = {
            active_border   = { colors = { "rgba(33ccffee)", "rgba(00ff99ee)" }, angle = 45 },
            inactive_border = "rgba(595959aa)",
        },

        resize_on_border = true,
        allow_tearing    = false,
        layout           = "master",
    },

    decoration = {
        rounding       = 0,
        rounding_power = 2,

        active_opacity   = 1.0,
        inactive_opacity = 0.8,

        shadow = {
            enabled      = true,
            range        = 4,
            render_power = 3,
            color        = "rgba(1a1a1aee)",
        },

        blur = {
            enabled  = true,
            size     = 3,
            passes   = 1,

            vibrancy = 0.1696,
        },
    },

    animations = {
        enabled = false,
    },
})

-- See https://wiki.hypr.land/Configuring/Layouts/Dwindle-Layout/ for more
hl.config({
    dwindle = {
        preserve_split = true, -- You probably want this
    },
})

-- See https://wiki.hypr.land/Configuring/Layouts/Master-Layout/ for more
hl.config({
    master = {
        new_status = "master",
    },
})

hl.config({
    misc = {
        force_default_wallpaper = 0,
        disable_hyprland_logo   = true,
    },
})


---------------
---- INPUT ----
---------------

hl.config({
    input = {
        kb_layout  = "us,ru",
        kb_variant = ",phonetic",
        kb_model   = "",
        kb_options = "grp:rctrl_toggle,grp_led:caps,ctrl:nocaps,compose:ralt",
        kb_rules   = "",

        follow_mouse = 1,

        sensitivity = 0, -- -1.0 - 1.0, 0 means no modification.

        touchpad = {
            natural_scroll = false,
        },
    },
})


---------------------
---- KEYBINDINGS ----
---------------------

local mainMod = "SUPER" -- Sets "Windows" key as main modifier

hl.bind(mainMod .. " + RETURN",    hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + SHIFT + C", hl.dsp.window.close())
hl.bind(mainMod .. " + Q",         hl.dsp.exec_cmd("hyprlock"))
hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.exit())
hl.bind(mainMod .. " + E",         hl.dsp.exec_cmd(fileManager))
hl.bind(mainMod .. " + SHIFT + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + D",         hl.dsp.exec_cmd(menu))
hl.bind(mainMod .. " + SHIFT + D", hl.dsp.exec_cmd(menu2))

hl.bind(mainMod .. " + SHIFT + S",   hl.dsp.exec_cmd("screenshot-copy"))
hl.bind(mainMod .. " + CONTROL + S", hl.dsp.exec_cmd("screenshot-edit"))

-- macOS-style shortcuts: Super acts like Cmd, translated to Ctrl for the app.
-- Copy/paste go through macos-key so terminals get Ctrl+Shift+C/V instead.
hl.bind(mainMod .. " + C",         hl.dsp.exec_cmd("macos-key C"))
hl.bind(mainMod .. " + V",         hl.dsp.exec_cmd("macos-key V"))
hl.bind(mainMod .. " + X",         hl.dsp.send_shortcut({ mods = "CTRL",       key = "X" }))
hl.bind(mainMod .. " + A",         hl.dsp.send_shortcut({ mods = "CTRL",       key = "A" }))
hl.bind(mainMod .. " + Z",         hl.dsp.send_shortcut({ mods = "CTRL",       key = "Z" }))
hl.bind(mainMod .. " + SHIFT + Z", hl.dsp.send_shortcut({ mods = "CTRL SHIFT", key = "Z" }))
hl.bind(mainMod .. " + F",         hl.dsp.send_shortcut({ mods = "CTRL",       key = "F" }))
hl.bind(mainMod .. " + W",         hl.dsp.send_shortcut({ mods = "CTRL",       key = "W" }))
hl.bind(mainMod .. " + N",         hl.dsp.send_shortcut({ mods = "CTRL",       key = "N" }))

hl.bind(mainMod .. " + left",  hl.dsp.focus({ direction = "l" }))
hl.bind(mainMod .. " + right", hl.dsp.focus({ direction = "r" }))
hl.bind(mainMod .. " + up",    hl.dsp.focus({ direction = "u" }))
hl.bind(mainMod .. " + down",  hl.dsp.focus({ direction = "d" }))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind(mainMod .. " + " .. key,             hl.dsp.focus({ workspace = i }))
    hl.bind(mainMod .. " + SHIFT + " .. key,     hl.dsp.window.move({ workspace = i }))
end

-- Example special workspace (scratchpad)
hl.bind(mainMod .. " + S", hl.dsp.workspace.toggle_special("magic"))
-- NOTE: this collided with the screenshot-copy bind above and was never active.
-- hl.bind(mainMod .. " + SHIFT + S", hl.dsp.window.move({ workspace = "special:magic" }))

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Laptop multimedia keys for volume and LCD brightness
hl.bind("XF86AudioRaiseVolume",  hl.dsp.exec_cmd("wpctl set-volume -l 2 @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume",  hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),      { locked = true, repeating = true })
hl.bind("XF86AudioMute",         hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),     { locked = true, repeating = true })
hl.bind("XF86AudioMicMute",      hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),   { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("brightnessctl set 5%+"),                          { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl set 5%-"),                          { locked = true, repeating = true })

hl.bind("XF86AudioNext",  hl.dsp.exec_cmd("playerctl next"),       { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay",  hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev",  hl.dsp.exec_cmd("playerctl previous"),   { locked = true })

-- Toggle the internal panel. These used to shell out to `hyprctl keyword monitor`;
-- in lua the monitor rule can just be re-applied directly.
hl.bind(mainMod .. " + XF86AudioMedia", function()
    hl.monitor({ output = "eDP-1", mode = "preferred", position = "0x0", scale = 2 })
    -- hl.monitor({ output = "eDP-1", mode = "preferred", position = "0x0", scale = 1.566667 })
end, { locked = true })

hl.bind(mainMod .. " + SHIFT + XF86AudioMedia", function()
    hl.monitor({ output = "eDP-1", disabled = true })
end, { locked = true })

hl.bind(mainMod .. " + ESCAPE", hl.dsp.focus({ workspace = "previous" }), { repeating = true })


--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

-- Example window rule
-- hl.window_rule({ name = "float-kitty", match = { class = "^(kitty)$", title = "^(kitty)$" }, float = true })

-- Ignore maximize requests from apps. You'll probably like this.
-- hl.window_rule({ name = "suppress-maximize-events", match = { class = ".*" }, suppress_event = "maximize" })

-- Fix some dragging issues with XWayland
-- hl.window_rule({
--     name  = "fix-xwayland-drags",
--     match = { class = "^$", title = "^$", xwayland = true, float = true, fullscreen = false, pin = false },
--     no_focus = true,
-- })


----------------
---- GROUPS ----
----------------

hl.config({
    group = {
        auto_group           = true, -- new windows join focused group
        insert_after_current = true,
        drag_into_group      = 1,    -- 0 disabled, 1 enabled, 2 only via the groupbar

        col = {
            border_active   = { colors = { "rgba(33ccffee)", "rgba(00ff99ee)" }, angle = 45 },
            border_inactive = "rgba(595959aa)",
        },

        groupbar = {
            enabled        = true,
            render_titles  = true,
            height         = 18,
            font_family    = "JetBrains Mono",
            font_size      = 11,
            gradients      = true,
            gaps_in        = 0,
            gaps_out       = 0,
            keep_upper_gap = false,

            col = {
                inactive = "rgba(000000aa)",
                active   = "rgba(595959aa)",
            },
        },
    },
})

hl.bind(mainMod .. " + T", hl.dsp.group.toggle())

hl.bind(mainMod .. " + L", hl.dsp.group.next())
hl.bind(mainMod .. " + H", hl.dsp.group.prev())

hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.move({ direction = "l", group_aware = true }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.move({ direction = "r", group_aware = true }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.move({ direction = "u", group_aware = true }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.move({ direction = "d", group_aware = true }))
hl.bind(mainMod .. " + U",         hl.dsp.window.move({ out_of_group = true }))

hl.bind("ALT + space", hl.dsp.exec_cmd("pkill -USR2 handy"))

-- hl.bind("switch:on:Lid Switch", hl.dsp.exec_cmd("hyprlock"), { locked = true })
-- hl.bind("XF86PowerOff",         hl.dsp.exec_cmd("hyprlock"), { locked = true })


-------------------
---- AUTOSTART ----
-------------------

hl.on("hyprland.start", function()
    hl.exec_cmd("dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP")
    hl.exec_cmd("systemctl --user import-environment {,WAYLAND_}DISPLAY SWAYSOCK")

    hl.exec_cmd("systemctl --user start waybar.service")
    hl.exec_cmd("systemctl --user start kanshi.service")
    hl.exec_cmd("systemctl --user start hypridle.service")
end)
