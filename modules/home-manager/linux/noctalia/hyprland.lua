-- Hyprland Lua configuration for Noctalia
-- Hyprland 0.55+ loads ~/.config/hypr/hyprland.lua.

-------------------
---- AUTOSTART ----
-------------------

hl.on("hyprland.start", function()
    hl.exec_cmd("noctalia")
end)

-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

hl.env("HYPRCURSOR_THEME", "rose-pine-hyprcursor")

---------------------
---- MY PROGRAMS ----
---------------------

local mainMod = os.getenv("NOCTALIA_HYPR_MOD") or "SUPER"

local terminal = "ghostty"
local browser = "default-browser"
local editor = "emacsclient -nc"
local fileManager = "thunar"
local voiceInput = os.getenv("HOME") .. "/Nix/assets/bin/voice-input.sh"

local sessionMenu = "noctalia msg panel-toggle session"
local lock = "noctalia msg session lock"
local launcher = "noctalia msg panel-toggle launcher"
local settings = "noctalia msg settings-toggle"
local controlCenter = "noctalia msg panel-toggle control-center"
local notificationsToggle = "noctalia msg notification-dnd-toggle"
local lightTheme = "noctalia msg theme-mode-set light"
local darkTheme = "noctalia msg theme-mode-set dark"
local screenshot = "noctalia msg screenshot-region"

local function cmd(command)
    return hl.dsp.exec_cmd(command)
end

----------------
---- INPUT  ----
----------------

hl.config({
    input = {
        natural_scroll = true,
        touchpad = {
            tap_to_click = false,
            natural_scroll = true,
            scroll_factor = 0.4,
        },
    },
    gestures = {
        workspace_swipe_distance = 100,
        workspace_swipe_direction_lock_threshold = 1000,
    },
})

hl.gesture({
    fingers = 3,
    direction = "vertical",
    action = function()
        hl.exec_cmd(controlCenter)
    end,
})

hl.gesture({
    fingers = 4,
    direction = "horizontal",
    action = "workspace",
})

-----------------------
---- LOOK AND FEEL ----
-----------------------

hl.config({
    general = {
        gaps_in = 0,
        gaps_out = 0,
        border_size = 0,
    },

    dwindle = {
        preserve_split = true,
    },

    decoration = {
        rounding = 0,
        rounding_power = 4.0,
        dim_inactive = true,
        dim_strength = 0.18,
        shadow = {
            enabled = false,
        },
        blur = {
            enabled = true,
            size = 8,
            passes = 2,
        },
    },

    animations = {
        enabled = true,
    },
})

hl.curve("snap", { type = "bezier", points = { { 0.05, 0.9 }, { 0.1, 1 } } })

hl.animation({ leaf = "windows", enabled = true, speed = 2, bezier = "snap" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 2, bezier = "snap" })
hl.animation({ leaf = "fade", enabled = true, speed = 1, bezier = "default" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 3, bezier = "snap" })
hl.animation({ leaf = "border", enabled = true, speed = 1, bezier = "default" })
hl.animation({ leaf = "borderangle", enabled = true, speed = 2, bezier = "default" })

---------------
---- RULES ----
---------------

hl.layer_rule({
    name = "noctalia",
    match = { namespace = "^noctalia-(bar-.+|notification|dock|panel|attached-panel|osd)$" },
    ignore_alpha = 0.5,
    blur = true,
    blur_popups = true,
})

hl.window_rule({
    name = "float-satty",
    match = { class = "com.gabm.satty" },
    float = true,
})

hl.window_rule({
    name = "float-thunar",
    match = { class = "^(thunar|Thunar|org\\.xfce\\.Thunar)$" },
    float = true,
    center = true,
})

---------------------
---- KEYBINDINGS ----
---------------------

-- Launchers / app controls
hl.bind(mainMod .. " + RETURN", cmd(terminal))
hl.bind(mainMod .. " + SHIFT + RETURN", cmd(browser))
hl.bind(mainMod .. " + Q", hl.dsp.window.close())
hl.bind(mainMod .. " + E", cmd(editor))
hl.bind(mainMod .. " + D", cmd(fileManager))
hl.bind(mainMod .. " + ESCAPE", cmd(lock))
hl.bind(mainMod .. " + SHIFT + ESCAPE", cmd(sessionMenu))
hl.bind(mainMod .. " + SPACE", cmd(launcher))
hl.bind(mainMod .. " + SHIFT + SPACE", cmd(controlCenter))
hl.bind(mainMod .. " + comma", cmd(settings))
hl.bind(mainMod .. " + F1", cmd(darkTheme))
hl.bind(mainMod .. " + F2", cmd(lightTheme))
hl.bind(mainMod .. " + SHIFT + S", cmd(screenshot))
hl.bind(mainMod .. " + N", cmd(notificationsToggle))
hl.bind(mainMod .. " + V", cmd(voiceInput))

-- Focus movement
hl.bind(mainMod .. " + h", hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + j", hl.dsp.focus({ direction = "down" }))
hl.bind(mainMod .. " + k", hl.dsp.focus({ direction = "up" }))
hl.bind(mainMod .. " + l", hl.dsp.focus({ direction = "right" }))

-- Window movement
hl.bind(mainMod .. " + SHIFT + h", hl.dsp.window.move({ direction = "left" }))
hl.bind(mainMod .. " + SHIFT + j", hl.dsp.window.move({ direction = "down" }))
hl.bind(mainMod .. " + SHIFT + k", hl.dsp.window.move({ direction = "up" }))
hl.bind(mainMod .. " + SHIFT + l", hl.dsp.window.move({ direction = "right" }))

-- Special workspace
hl.bind(mainMod .. " + m", hl.dsp.workspace.toggle_special("magic"))
hl.bind(mainMod .. " + SHIFT + m", hl.dsp.window.move({ workspace = "special:magic" }))

-- Other bindings
hl.bind(mainMod .. " + p", hl.dsp.focus({ workspace = "previous" }))
hl.bind(mainMod .. " + o", hl.dsp.layout("togglesplit"))
hl.bind(mainMod .. " + f", hl.dsp.window.float({ action = "toggle" }))

-- Tab navigation in grouped windows.
hl.bind(mainMod .. " + t", hl.dsp.group.toggle())
hl.bind(mainMod .. " + TAB", hl.dsp.group.next())
hl.bind(mainMod .. " + SHIFT + TAB", hl.dsp.group.prev())

-- Workspace switching / moving
for i = 1, 10 do
    local key = i % 10
    hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ workspace = i }))
    hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

-- Mouse bindings
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.resize(), { mouse = true })
hl.bind(mainMod .. " + SHIFT + mouse:272", hl.dsp.window.drag(), { mouse = true })

-- Volume
hl.bind("XF86AudioRaiseVolume", cmd("noctalia msg volume-up"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", cmd("noctalia msg volume-down"), { locked = true, repeating = true })
hl.bind("XF86AudioMute", cmd("noctalia msg volume-mute"), { locked = true })

-- Brightness (Noctalia)
hl.bind("XF86MonBrightnessUp", cmd("noctalia msg brightness-up"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", cmd("noctalia msg brightness-down"), { locked = true, repeating = true })

-- Media (Noctalia)
hl.bind("XF86AudioPlay", cmd("noctalia msg media toggle"), { locked = true })
hl.bind("XF86AudioNext", cmd("noctalia msg media next"), { locked = true })
hl.bind("XF86AudioPrev", cmd("noctalia msg media previous"), { locked = true })
