-- Standard awesome library
require("awful")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

-- Load Debian menu entries
require("debian.menu")

-- {{{ Variable definitions
-- This is a file path to a theme file which will defines colors.
-- theme_path = "/usr/local/share/awesome/themes/default"
-- theme_path = "/usr/local/share/awesome/themes/default/theme"
-- theme_path = "/usr/local/share/awesome/themes/sky/theme"
theme_path = os.getenv("HOME") .. "/.config/awesome/themes/slk-3.1"

-- Actually load theme
beautiful.init(theme_path)

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "nano"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
-- modkey = "Mod4"
modkey = "Mod3"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.magnifier,
    awful.layout.suit.floating
}

-- Table of clients that should be set floating. The index may be either
-- the application class or instance. The instance is useful when running
-- a console app in a terminal like (Music on Console)
--    xterm -name mocp -e mocp
floatapps =
{
    -- by class
    ["MPlayer"] = true,
    ["pinentry"] = true,
    ["gimp"] = true,
    -- by instance
    ["mocp"] = true
}

-- Applications to be moved to a pre-defined tag by class or instance.
-- Use the screen and tags indices.
apptags =
{
    ["Firefox"] = { screen = 1, tag = 1 },
    ["Iceweasel"] = { screen = 1, tag = 1 },
    ["Emacs"] = { screen = 1, tag = 5 },
    ["Pidgin"] = { screen = screen.count(), tag = 9 },
    ["Stardict"] = { screen = screen.count(), tag = 9 },
    ["Quodlibet"] = { screen = screen.count(), tag = 8 },
    -- ["mocp"] = { screen = 2, tag = 4 },
}

-- Define if we want to use titlebar on all applications.
use_titlebar = false
-- }}}

-- {{{ Tags
-- Define tags table.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = {}
    -- Create 9 tags per screen.
    for tagnumber = 1, 9 do
        tags[s][tagnumber] = tag(tagnumber)
        -- Add tags to screen one by one
        tags[s][tagnumber].screen = s
        awful.layout.set(layouts[1], tags[s][tagnumber])
    end
    -- I'm sure you want to see at least one tag.
    tags[s][1].selected = true
end
-- }}}

-- Set tile layout for the 9th tag on the rightemost screen
tags[screen.count()][9].layout = "tile"
tags[screen.count()][9].mwfact = 0.7

-- {{{ Wibox
-- Create a textbox widget
mytextbox = widget({ type = "textbox", align = "right" })
-- Set the default text in textbox
mytextbox.text = "<b><small> " .. AWESOME_RELEASE .. " </small></b>"

-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu.new({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                        { "open terminal", terminal },
                                        { "Debian", debian.menu.Debian_menu.Debian }
                                      }
                            })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })

-- Create a systray
mysystray = widget({ type = "systray", align = "right" })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = { button({ }, 1, awful.tag.viewonly),
                      button({ modkey }, 1, awful.client.movetotag),
                      button({ }, 3, function (tag) tag.selected = not tag.selected end),
                      button({ modkey }, 3, awful.client.toggletag),
                      button({ }, 4, awful.tag.viewnext),
                      button({ }, 5, awful.tag.viewprev) }
mytasklist = {}
mytasklist.buttons = { button({ }, 1, function (c) client.focus = c; c:raise() end),
                       button({ }, 3, function () if instance then instance:hide() end instance = awful.menu.clients({ width=250 }) end),
                       button({ }, 4, function () awful.client.focus.byidx(1) end),
                       button({ }, 5, function () awful.client.focus.byidx(-1) end) }

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = widget({ type = "textbox", align = "left" })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = widget({ type = "imagebox", align = "left" })
    mylayoutbox[s]:buttons({ button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                             button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                             button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                             button({ }, 5, function () awful.layout.inc(layouts, -1) end) })
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist.new(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist.new(function(c)
                                                  return awful.widget.tasklist.label.currenttags(c, s)
                                              end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = wibox({ position = "top", fg = beautiful.fg_normal, bg = beautiful.bg_normal })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = { mylauncher,
                           mytaglist[s],
                           mytasklist[s],
                           mypromptbox[s],
                           s == screen.count() and mysystray or nil,
                           mytextbox,
                           mylayoutbox[s],}
    mywibox[s].screen = s
end
-- }}}

-- {{{ Mouse bindings
root.buttons({
    button({ }, 3, function () mymainmenu:toggle() end),
    button({ }, 4, awful.tag.viewnext),
    button({ }, 5, awful.tag.viewprev)
})
-- }}}

-- {{{ Key bindings
-- Bind keyboard digits
-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

globalkeys = {}
clientkeys = {}

for i = 1, keynumber do
    table.insert(globalkeys,
        key({ modkey }, i,
            function ()
                local screen = mouse.screen
                if tags[screen][i] then
                    awful.tag.viewonly(tags[screen][i])
                end
            end))
    table.insert(globalkeys,
        key({ modkey, "Control" }, i,
            function ()
                local screen = mouse.screen
                if tags[screen][i] then
                    tags[screen][i].selected = not tags[screen][i].selected
                end
            end))
    table.insert(globalkeys,
        key({ modkey, "Shift" }, i,
            function ()
                if client.focus and tags[client.focus.screen][i] then
                    awful.client.movetotag(tags[client.focus.screen][i])
                end
            end))
    table.insert(globalkeys,
        key({ modkey, "Control", "Shift" }, i,
            function ()
                if client.focus and tags[client.focus.screen][i] then
                    awful.client.toggletag(tags[client.focus.screen][i])
                end
            end))
end

table.insert(globalkeys, key({ modkey }, "Left", awful.tag.viewprev))
table.insert(globalkeys, key({ modkey }, "Right", awful.tag.viewnext))
table.insert(globalkeys, key({ modkey }, "Escape", awful.tag.history.restore))

table.insert(globalkeys, key({ modkey }, "j", function () awful.client.focus.byidx(1); if client.focus then client.focus:raise() end end))
table.insert(globalkeys, key({ modkey }, "k", function () awful.client.focus.byidx(-1);  if client.focus then client.focus:raise() end end))
table.insert(globalkeys, key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(1) end))
table.insert(globalkeys, key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx(-1) end))

table.insert(globalkeys, key({ modkey, "Control" }, "j", function () awful.screen.focus(1) end))
table.insert(globalkeys, key({ modkey, "Control" }, "k", function () awful.screen.focus(-1) end))

table.insert(globalkeys, key({ modkey }, "Tab", function () awful.client.focus.history.previous(); if client.focus then client.focus:raise() end end))

table.insert(globalkeys, key({ modkey }, "u", awful.client.urgent.jumpto))

-- Standard program
table.insert(globalkeys, key({ modkey }, "t", function () awful.util.spawn(terminal) end))
table.insert(globalkeys, key({ modkey }, "space", function () awful.util.spawn("quodlibet --play-pause") end))
table.insert(globalkeys, key({ modkey }, "F1", function () awful.util.spawn("eee2ram.sh") end))
table.insert(globalkeys, key({ modkey }, "F3", function () awful.util.spawn("quodlibet --previous") end))
table.insert(globalkeys, key({ modkey }, "F4", function () awful.util.spawn("quodlibet --next") end))
table.insert(globalkeys, key({ modkey }, "F5", function () awful.util.spawn("xrandr-eee.py auto") end))
toggle_mute_cmd = "amixer get LineOut | grep off && amixer set LineOut unmute || amixer set LineOut mute"
table.insert(globalkeys, key({ modkey }, "F7", function () awful.util.spawn(toggle_mute_cmd) end))
table.insert(globalkeys, key({ modkey }, "F8", function () awful.util.spawn("amixer set PCM 2dB-") end))
table.insert(globalkeys, key({ modkey }, "F9", function () awful.util.spawn("amixer set PCM 2dB+") end))
table.insert(globalkeys, key({ modkey }, "F12", function () awful.util.spawn('xlock') end))

table.insert(globalkeys, key({ modkey, "Control" }, "r", function ()
                                                             mypromptbox[mouse.screen].text =
                                                                 awful.util.escape(awful.util.restart())
                                                          end))
table.insert(globalkeys, key({ modkey, "Shift" }, "q", awesome.quit))

-- Client manipulation
table.insert(clientkeys, key({ modkey }, "m", function (c) c.maximized_horizontal = not c.maximized_horizontal
                                                           c.maximized_vertical = not c.maximized_vertical end))
table.insert(clientkeys, key({ modkey }, "f", function (c) c.fullscreen = not c.fullscreen end))
--table.insert(clientkeys, key({ modkey, "Shift" }, "c", function (c) c:kill() end))
table.insert(clientkeys, key({ modkey }, "c", function (c) c:kill() end))
table.insert(clientkeys, key({ modkey, "Control" }, "space", awful.client.floating.toggle))
table.insert(clientkeys, key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end))
table.insert(clientkeys, key({ modkey }, "o", awful.client.movetoscreen))
table.insert(clientkeys, key({ modkey, "Shift" }, "r", function (c) c:redraw() end))

-- Layout manipulation
table.insert(globalkeys, key({ modkey }, "l", function () awful.tag.incmwfact(0.05) end))
table.insert(globalkeys, key({ modkey }, "h", function () awful.tag.incmwfact(-0.05) end))
table.insert(globalkeys, key({ modkey, "Shift" }, "h", function () awful.tag.incnmaster(1) end))
table.insert(globalkeys, key({ modkey, "Shift" }, "l", function () awful.tag.incnmaster(-1) end))
table.insert(globalkeys, key({ modkey, "Control" }, "h", function () awful.tag.incncol(1) end))
table.insert(globalkeys, key({ modkey, "Control" }, "l", function () awful.tag.incncol(-1) end))
--table.insert(globalkeys, key({ modkey }, "space", function () awful.layout.inc(layouts, 1) end))
--table.insert(globalkeys, key({ modkey, "Shift" }, "space", function () awful.layout.inc(layouts, -1) end))
table.insert(globalkeys, key({ modkey }, "v", function () awful.layout.inc(layouts, 1) end))
table.insert(globalkeys, key({ modkey, "Shift" }, "v", function () awful.layout.inc(layouts, -1) end))


-- Prompt
table.insert(globalkeys, key({ modkey }, "p", function ()
                                                    awful.prompt.run({ prompt = "Run: " },
                                                                     mypromptbox[mouse.screen],
                                                                     awful.util.spawn, awful.completion.bash,
                                                                     awful.util.getdir("cache") .. "/history")
                                               end))
table.insert(globalkeys, key({ modkey }, "F10", function ()
                                                   awful.prompt.run({ prompt = "Run Lua code: " },
                                                                    mypromptbox[mouse.screen],
                                                                    awful.util.eval, awful.prompt.bash,
                                                                    awful.util.getdir("cache") .. "/history_eval")
                                               end))

table.insert(globalkeys, key({ modkey, "Ctrl" }, "i", function ()
                                        local s = mouse.screen
                                        if mypromptbox[s].text then
                                            mypromptbox[s].text = nil
                                        else
                                            mypromptbox[s].text = nil
                                            if client.focus.class then
                                                mypromptbox[s].text = "Class: " .. client.focus.class .. " "
                                            end
                                            if client.focus.instance then
                                                mypromptbox[s].text = mypromptbox[s].text .. "Instance: ".. client.focus.instance .. " "
                                            end
                                            if client.focus.role then
                                                mypromptbox[s].text = mypromptbox[s].text .. "Role: ".. client.focus.role
                                            end
                                        end
                                    end))

-- Client awful tagging: this is useful to tag some clients and then do stuff like move to tag on them
table.insert(clientkeys, key({ modkey }, "Return", awful.client.togglemarked))

for i = 1, keynumber do
    table.insert(globalkeys, key({ modkey, "Shift" }, "F" .. i,
                 function ()
                     local screen = mouse.screen
                     if tags[screen][i] then
                         for k, c in pairs(awful.client.getmarked()) do
                             awful.client.movetotag(tags[screen][i], c)
                         end
                     end
                 end))
end

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Hooks
-- Hook function to execute when focusing a client.
awful.hooks.focus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_focus
    end
end)

-- Hook function to execute when unfocusing a client.
awful.hooks.unfocus.register(function (c)
    if not awful.client.ismarked(c) then
        c.border_color = beautiful.border_normal
    end
end)

-- Hook function to execute when marking a client
awful.hooks.marked.register(function (c)
    c.border_color = beautiful.border_marked
end)

-- Hook function to execute when unmarking a client.
awful.hooks.unmarked.register(function (c)
    c.border_color = beautiful.border_focus
end)

-- Hook function to execute when the mouse enters a client.
awful.hooks.mouse_enter.register(function (c)
    -- Sloppy focus, but disabled for magnifier layout
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

-- Hook function to execute when a new client appears.
awful.hooks.manage.register(function (c, startup)
    -- If we are not managing this application at startup,
    -- move it to the screen where the mouse is.
    -- We only do it for filtered windows (i.e. no dock, etc).
    if not startup and awful.client.focus.filter(c) then
        c.screen = mouse.screen
    end

    if use_titlebar then
        -- Add a titlebar
        awful.titlebar.add(c, { modkey = modkey })
    end
    -- Add mouse bindings
    c:buttons({
        button({ }, 1, function (c) client.focus = c; c:raise() end),
        button({ modkey }, 1, awful.mouse.client.move),
        button({ modkey }, 3, awful.mouse.client.resize)
    })
    -- New client may not receive focus
    -- if they're not focusable, so set border anyway.
    c.border_width = beautiful.border_width
    c.border_color = beautiful.border_normal

    -- Check if the application should be floating.
    local cls = c.class
    local inst = c.instance
    if floatapps[cls] then
        awful.client.floating.set(c, floatapps[cls])
    elseif floatapps[inst] then
        awful.client.floating.set(c, floatapps[inst])
    end

    -- Check application->screen/tag mappings.
    local target
    if apptags[cls] then
        target = apptags[cls]
    elseif apptags[inst] then
        target = apptags[inst]
    end
    if target then
        c.screen = target.screen
        awful.client.movetotag(tags[target.screen][target.tag], c)
    end

    -- Do this after tag mapping, so you don't see it on the wrong tag for a split second.
    client.focus = c

    -- Set key bindings
    c:keys(clientkeys)

    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- awful.client.setslave(c)

    -- Honor size hints: if you want to drop the gaps between windows, set this to false.
    c.size_hints_honor = false
end)

-- Hook function to execute when arranging the screen.
-- (tag switch, new client, etc)
awful.hooks.arrange.register(function (screen)
    local layout = awful.layout.getname(awful.layout.get(screen))
    if layout and beautiful["layout_" ..layout] then
        mylayoutbox[screen].image = image(beautiful["layout_" .. layout])
    else
        mylayoutbox[screen].image = nil
    end

    -- Give focus to the latest client in history if no window has focus
    -- or if the current window is a desktop or a dock one.
    if not client.focus then
        local c = awful.client.focus.history.get(screen, 0)
        if c then client.focus = c end
    end
end)

-- Hook called every second
awful.hooks.timer.register(1, function ()
    -- For unix time_t lovers
    -- mytextbox.text = " " .. os.time() .. " time_t "
    -- Otherwise use:
    mytextbox.text = " " .. os.date() .. " "
end)
-- }}}

-- {{{ My custom keybindings
-- Create a list of keybindings to be added when mod-u is pressed
keybind_mod_u = {}

function keychain_mod_u_add()
    for k, v in pairs(keybind_mod_u) do
        v:add()
    end
end

function keychain_mod_u_remove()
    for k, v in pairs(keybind_mod_u) do
        v:remove()
    end
end

mod_u_commands = {q = "quodlibet",
                  m = terminal .. " -e mutt",
                  a = "xterm -e alpine",
                  e = "emacs",
                  p = "pidgin",
                  f = "firefox",
                  d = terminal .. " -e sudo aptitude",
                  v = "vagalume",
                  i = "icedove",
                  s = "stardict",
                  y = terminal .. " -e ipython"}

for key, cmd in pairs(mod_u_commands) do
   table.insert(keybind_mod_u,
                keybinding({}, key, function ()
                                       awful.util.spawn(cmd)
                                       keychain_mod_u_remove()
                                    end))
end

-- This is an example keybinding with an additional modifier
table.insert(keybind_mod_u, keybinding({ "Mod4" }, "b", function ()
    mytextbox.text = "You pressed Mod4 + b!"
    keychain_mod_u_remove()
end))

-- Escape key cancels
table.insert(keybind_mod_u, keybinding({}, "Escape", keychain_mod_u_remove))

keybinding({ modkey }, "u", keychain_mod_u_add):add()

-- Execute a shell command and return it's output
function execute_command(cmd)
   local f = io.popen(cmd)
   local out = f:read("*a")
   f:close()
   return out
end

-- Translate current selection using dict and naughty
function translate()
   local clip = execute_command("xclip -o")
   local output = execute_command('dict -d mueller7 ' .. clip .. '| tail -n+6')
   naughty.notify({ title = clip,
                    text = "<tt>" .. output .. "</tt>",
                    timeout = 10, width = 600, fg='#FFFFFF' })
end
table.insert(globalkeys, key({ modkey }, "F2", translate))

-- Open selected link in a browser
function open_link()
   local clip = execute_command("xclip -o")
   naughty.notify({ title="Opening link",
                    text=clip,
                    timeout = 3 })
   awful.util.spawn("firefox -new-tab " .. clip)
end
table.insert(globalkeys, key({ modkey }, "F11", open_link))

-- }}}
