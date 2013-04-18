-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

-- Load Debian menu entries
require("debian.menu")

require("obvious.battery")
require("vicious")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(os.getenv("HOME") .. "/.config/awesome/themes/cac2s/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "x-terminal-emulator"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- browser = "iceweasel -new-tab"
browser = "chromium"
screenlock = "i3lock -c 330033 -b -d"
screenlock_sleep = "i3lock -c 330033 -b"
emacs = "emacs24"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.floating
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
if screen.count() == 2 then
tags = awful.util.table.join(
   awful.tag({1, 2, 3, 4, 5}, 1, awful.layout.suit.max),
   awful.tag({6, 7, 8, 9}, 2, awful.layout.suit.max))
   awful.layout.set(awful.layout.suit.tile, tags[9])
   awful.tag.setmwfact(0.7, tags[9])
else
   tags = {}
   for s = 1, screen.count() do
      -- Each screen has its own tag table.
      tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, awful.layout.suit.max)
      awful.layout.set(awful.layout.suit.tile, tags[s][9])
      awful.tag.setmwfact(0.7, tags[s][9])
   end
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "Debian", debian.menu.Debian_menu.Debian },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })

-- Create a systray
mysystray = widget({ type = "systray" })

-- Volume
colorWidgetFg = "#999999"
textVolume = widget({ type = "textbox" })
-- textVolume.bg_image = image(awful.util.getdir("config") .. "/icons/vol.png")
textVolume.bg_align = "left"
textVolume.align = "right"
vicious.cache(vicious.widgets.volume)
vicious.register(textVolume, vicious.widgets.volume,
                function (widget, args)
                    if args[2] == "♫" then
                        return '<span color="#00c000"> ♫ </span>' .. string.format("%02d", args[1]) .. '<span color="' .. colorWidgetFg .. '">%</span>'
                    else
                        return '<span color="' .. theme.bg_urgent .. '"> ♫ ' .. string.format("%02d", args[1]) .. '%</span>'
                    end
                end,
                60, "Master")
textVolume:buttons(awful.util.table.join(
    awful.button({ }, 2,
        function ()
            awful.util.spawn("amixer -q sset Master toggle")
            vicious.force({ textVolume })
        end
    ),
    awful.button({ }, 4,
        function ()
            awful.util.spawn("amixer -q sset Master 1%+")
            vicious.force({ textVolume })
        end
    ),
    awful.button({ }, 5,
        function ()
            awful.util.spawn("amixer -q sset Master 1%-")
            vicious.force({ textVolume })
        end
    )
))

-- Weather
weatherwidget = widget({ type = "textbox" })
-- ICAO code for Pulkovo is ULLI
-- ICAO code for Nice is LFMN
vicious.register(weatherwidget, vicious.widgets.weather, ' <span color="#00c000">☼</span> ${tempc}°C ${windkmh} km/h', 1800, 'LFMN')

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if not c:isvisible() then
                                                  awful.tag.viewonly(c:tags()[1])
                                              end
                                              client.focus = c
                                              c:raise()
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mypromptbox[s],
            mylayoutbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mytextclock,
        textVolume,
        weatherwidget,
        obvious.battery(),
        s == 1 and mysystray or nil,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

function move_left ()
   local screen = mouse.screen
   local tag = awful.tag.selected(screen)
   if tag == tags[6] then
      awful.screen.focus(1)
      awful.tag.viewonly(tags[5])
   else
      awful.tag.viewprev()
   end
end

function move_right ()
   local screen = mouse.screen
   local tag = awful.tag.selected(screen)
   if tag == tags[5] then
      awful.screen.focus(2)
      awful.tag.viewonly(tags[6])
   else
      awful.tag.viewnext()
   end
end

-- {{{ Key bindings
globalkeys = awful.util.table.join(
   awful.key({ modkey,           }, "Left",   move_left),
   awful.key({ modkey,           }, "Right",  move_right),
   awful.key({ modkey,           }, "b",   move_left),
   awful.key({ modkey,           }, "f",  move_right),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show(true)        end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "Down", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey,           }, "Up", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "n", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey,           }, "p", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end)
)

-- http://awesome.naquadah.org/wiki/FullScreens
function fullscreens(c)
   awful.client.floating.toggle(c)
   if awful.client.floating.get(c) then
      local clientX = screen[1].workarea.x
      local clientY = screen[1].workarea.y
      local clientWidth = 0
      -- look at http://www.rpm.org/api/4.4.2.2/llimits_8h-source.html
      local clientHeight = 2147483640
      for s = 1, screen.count() do
         clientHeight = math.min(clientHeight, screen[s].workarea.height)
         clientWidth = clientWidth + screen[s].workarea.width
      end
      local t = c:geometry({x = clientX, y = clientY, width = clientWidth, height = clientHeight})
   else
      --apply the rules to this client so he can return to the right tag if there is a rule for that.
      awful.rules.apply(c)
   end
   -- focus our client
   client.focus = c
   c:raise()
end

clientkeys = awful.util.table.join(
    awful.key({ modkey, "Shift"   }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Control" }, "f",      fullscreens),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey, "Shift"   }, "n",      function (c) c.minimized = not c.minimized    end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

if screen.count() == 2 then
   for i = 1, 9 do
      globalkeys = awful.util.table.join(globalkeys,
                                         awful.key({ modkey }, "#" .. i + 9,
                                                   function ()
                                                      local screen = mouse.screen
                                                      if screen == 2 and (i == 1 or i == 2 or i == 3 or i == 4) then
                                                         awful.screen.focus(1)
                                                      end
                                                      if screen == 1 and (i==5 or i == 6 or i == 7 or i == 8 or i == 9) then
                                                         awful.screen.focus(2)
                                                      end
                                                      awful.tag.viewonly(tags[i])
                                                   end),
                                         awful.key({ modkey, "Control" }, "#" .. i + 9,
                                                   function ()
                                                      awful.tag.viewtoggle(tags[i])
                                                   end),
                                         awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                                   function ()
                                                      if client.focus then
                                                         awful.client.movetotag(tags[i])
                                                      end
                                                   end),
                                         awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                                   function ()
                                                      if client.focus and tags[i] then
                                                         awful.client.toggletag(tags[i])
                                                      end
                                                   end))
   end
else
   -- Compute the maximum number of digit we need, limited to 9
   keynumber = 0
   for s = 1, screen.count() do
      keynumber = math.min(9, math.max(#tags[s], keynumber));
   end

   -- Bind all key numbers to tags.
   -- Be careful: we use keycodes to make it works on any keyboard layout.
   -- This should map on the top row of your keyboard, usually 1 to 9.
   for i = 1, keynumber do
      globalkeys = awful.util.table.join(globalkeys,
                                         awful.key({ modkey }, "#" .. i + 9,
                                                   function ()
                                                      local screen = mouse.screen
                                                      if tags[screen][i] then
                                                         awful.tag.viewonly(tags[screen][i])
                                                      end
                                                   end),
                                         awful.key({ modkey, "Control" }, "#" .. i + 9,
                                                   function ()
                                                      local screen = mouse.screen
                                                      if tags[screen][i] then
                                                         awful.tag.viewtoggle(tags[screen][i])
                                                      end
                                                   end),
                                         awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                                   function ()
                                                      if client.focus and tags[client.focus.screen][i] then
                                                         awful.client.movetotag(tags[client.focus.screen][i])
                                                      end
                                                   end),
                                         awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                                   function ()
                                                      if client.focus and tags[client.focus.screen][i] then
                                                         awful.client.toggletag(tags[client.focus.screen][i])
                                                      end
                                                   end))
   end
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- {{{ My keybindings
function execute_command(cmd)
   local f = io.popen(cmd)
   local out = f:read("*a")
   f:close()
   return out
end

function translate(db)
   local clip = execute_command("xsel")
   local output = execute_command('dict -d ' .. db .. " " .. clip .. '| tail -n+6')
   naughty.notify({ title = clip,
                    text = "<tt>" .. output .. "</tt>",
                    timeout = 10, width = 600, fg='#FFFFFF' })
end

function browse_url()
   local clip = execute_command("xsel")
   naughty.notify({ title="Opening link",
                    text=clip,
                    timeout = 3 })
   awful.util.spawn(browser .. " " .. clip)
end

globalkeys = awful.util.table.join(globalkeys,
                                   awful.key({ modkey }, "F12", function()
                                                                   translate("wn")
                                                                end),
                                   awful.key({ modkey, "Shift" }, "F12", function()
                                                                   translate("mueller7")
                                                                end),
                                   awful.key({ modkey }, "F11", browse_url),
                                   awful.key({ modkey }, "F2", function () awful.util.spawn(screenlock) end),
                                   awful.key({ }, "XF86Sleep", function () awful.util.spawn(screenlock_sleep) end),
                                   awful.key({ }, "XF86Launch1", function ()
                                                                    awful.tag.viewonly(tags[1][5])
                                                                    run_or_raise(emacs, { class = "Emacs" })
                                                                 end),
                                   awful.key({ }, "XF86AudioMute", function () awful.util.spawn("amixer -q sset Master toggle") end),
                                   awful.key({ }, "XF86AudioRaiseVolume", function ()
                                                                             awful.util.spawn("amixer -q sset Master 2%+")
                                                                             vicious.force({ textVolume })
                                                                          end),
                                   awful.key({ }, "XF86AudioLowerVolume", function ()
                                                                             awful.util.spawn("amixer -q sset Master 2%-")
                                                                             vicious.force({ textVolume })
                                                                          end)
)

-- }}}

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][1] } },
    -- { rule = { class = "Iceweasel" },
    --   properties = { tag = tags[1][1] } },
    { rule = { class = "Pidgin" },
      properties = { tag = tags[1][9] } },
    { rule = { class = "Emacs" },
      properties = { tag = tags[1][5] } },
    -- { rule = { class = "Gajim.py" },
    --   properties = { tag = tags[1][9] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if awful.rules.match(c, { class = "Eclipse" }) then
       fullscreens(c)
    end

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

--- Spawns cmd if no client can be found matching properties
-- If such a client can be found, pop to first tag where it is visible, and give it focus
-- @param cmd the command to execute
-- @param properties a table of properties to match against clients.  Possible entries: any properties of the client object
function run_or_raise(cmd, properties)
   local clients = client.get()
   local focused = awful.client.next(0)
   local findex = 0
   local matched_clients = {}
   local n = 0
   for i, c in pairs(clients) do
      --make an array of matched clients
      if match(properties, c) then
         n = n + 1
         matched_clients[n] = c
         if c == focused then
            findex = n
         end
      end
   end
   if n > 0 then
      local c = matched_clients[1]
      -- if the focused window matched switch focus to next in list
      if 0 < findex and findex < n then
         c = matched_clients[findex+1]
      end
      local ctags = c:tags()
      if table.getn(ctags) == 0 then
         -- ctags is empty, show client on current tag
         local curtag = awful.tag.selected()
         awful.client.movetotag(curtag, c)
      else
         -- Otherwise, pop to first tag client is visible on
         awful.tag.viewonly(ctags[1])
      end
      -- And then focus the client
      client.focus = c
      c:raise()
      return
   end
   awful.util.spawn(cmd)
end

-- Returns true if all pairs in table1 are present in table2
function match (table1, table2)
   for k, v in pairs(table1) do
      if table2[k] ~= v and not table2[k]:find(v) then
         return false
      end
   end
   return true
end

-- disable startup-notification globally
local oldspawn = awful.util.spawn
awful.util.spawn = function (s)
  oldspawn(s, false)
end
