-- Imports {{{
import XMonad
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Actions.SpawnOn
import XMonad.Util.Paste
import XMonad.Actions.FloatKeys
import XMonad.Actions.OnScreen

import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Ratio ((%))
-- }}}

-- Set Terminal; Border colour/width; modmask; workspace names {{{
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt +bc +uc -cr Green"

-- Width of the window border in pixels.
--
myBorderWidth   = 0

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
myWorkspaces    = ["1:tmux","2:tmux","3:web","4:IM","5:thunar","6:email","7:spotify"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"
-- }}}

-- Key bindings. Add, modify or remove key bindings here. {{{
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((0, xK_Super_L      ), kill)
    , ((0, xK_Alt_R      ), windows $ viewOnScreen 0 "2:tmux" . viewOnScreen 1 "1:tmux")
    , ((0, xK_Super_R      ), windows $ viewOnScreen 0 "3:web" . viewOnScreen 1 "1:tmux")
--    , ((0, xK_Menu      ), windows $ viewOnScreen 0 "4:IM" . viewOnScreen 1 "1:tmux")
    , ((0, xK_Menu      ), windows $ viewOnScreen 0 "4:IM" . viewOnScreen 1 "6:email")
    , ((0, xK_Control_R      ), windows $ viewOnScreen 0 "5:thunar" . viewOnScreen 1 "1:tmux")
    , ((modm, xK_Control_R      ), windows $ viewOnScreen 0 "7:spotify" . viewOnScreen 1 "1:tmux")

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
    -- , ((modm , xK_b ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)

    , ((modm,               xK_Down),  nextWS)
    , ((modm,               xK_Up),    prevWS)
    , ((modm .|. shiftMask, xK_Down),  shiftToNext)
    , ((modm .|. shiftMask, xK_Up),    shiftToPrev)
    , ((modm,               xK_Right), nextScreen)
    , ((modm,               xK_Left),  prevScreen)
    , ((modm .|. shiftMask, xK_Right), shiftNextScreen)
    , ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
    , ((modm,               xK_z),     toggleWS)

    , ((modm, xK_c),    spawn "tmux show-buffer | xclip -i -selection clipboard")

    , ((modm, xK_v),    spawn "tmux set-buffer -- \"$(xclip -o -selection clipboard)\"; tmux paste-buffer")
    , ((modm .|. shiftMask, xK_l),    spawn "xscreensaver-command -lock")
    , ((0, xK_Pause),    spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")

    , ((modm,               xK_d     ), withFocused (keysResizeWindow (-10,-10) (1, 1)))
    , ((modm,               xK_s     ), withFocused (keysResizeWindow (10,10) (1,1)))
    , ((modm .|. shiftMask, xK_d     ), withFocused (keysAbsResizeWindow (-10,-10) (1024,752)))
    , ((modm .|. shiftMask, xK_s     ), withFocused (keysAbsResizeWindow (10,10) (1024,752)))
    , ((modm,               xK_a     ), withFocused (keysMoveWindowTo (512,384) (1%2,1%2)))

    -- Decrease/Increase volume.
    , ((0, xK_Print), spawn "amixer -q set Master 2%- && amixer -q set Front 2%-")

    , ((0, xK_Scroll_Lock), spawn "amixer -q set Master 2%+ && amixer -q set Front 2%+")

    ]

    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------ }}}

-- Mouse bindings: default actions bound to mouse events {{{
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
-- }}}

-- Layouts {{{

-- You can specify and transform your layouts by modifying these values.xmproc
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full
--  where
--     -- default tiling algorithm partitions the screen into two panes
--     tiled   = Tall nmaster delta ratio
--
--     -- The default number of windows in the master pane
--     nmaster = 1
--
--     -- Default proportion of screen occupied by master pane
--     ratio   = 1/2
--
--     -- Percent of screen to increment by when resizing panes
--     delta   = 3/100
myLayout  =  spacing 7                                                          $
             onWorkspaces ["1:tmux", "7:spotify" ]                    allLayout $
             onWorkspaces ["2:tmux", "3:web", "5:thunar", "6:email"] tallLayout $
             onWorkspaces ["4:IM"]                                     imLayout $
             allLayout
-- Layout
tallLayout = avoidStruts $ Full ||| tiled --- ||| simpleFloat
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

allLayout = avoidStruts $ Full ||| tiled ||| Mirror tiled --- ||| simpleFloat
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

imLayout = avoidStruts $ smartBorders $ withIM ratio pidginRoster $ reflectHoriz $ withIM skypeRatio skypeRoster (Full ||| Grid)
  where
    chatLayout      = Grid
    ratio           = (1%9)
    skypeRatio      = (1%8)
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
    skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

-- }}}

-- Window rules; Shifting them to correct place {{{

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
--myManageHook = composeAll
--    [ className =? "MPlayer"        --> doFloat
--    , className =? "Gimp"           --> doFloat
--    , resource  =? "desktop_window" --> doIgnore
--    , resource  =? "stalonetray" --> doIgnore
--    , resource  =? "kdesktop"       --> doIgnore ]
myManageHook = (composeAll . concat $
    [ [resource     =? r          --> doIgnore             |   r   <- myIgnores]
      -- ignore desktop
    , [appName      =? c          --> doShift  "1:tmux"    |   c   <- myTmx1   ]     -- move dev to main
    , [className    =? c          --> doShift  "2:tmux"    |   c   <- myTmx2   ]     -- move webs to main
    , [className    =? c          --> doShift  "3:web"     |   c   <- myWebs   ]     -- move webs to main
    , [className    =? c          --> doShift  "4:IM"      |   c   <- myChat   ]
      -- move chat to chat
    , [className    =? c          --> doShift  "5:thunar"  |   c   <- myThunar ]     -- move img to div
    , [className    =? c          --> doShift  "7:spotify" |   c   <- myMusic  ]     -- move music to music
    , [className    =?           "Orage" --> doFloatAt (1/1680) (1-176/1050) ]
    , [className    =?           "Thunderbird" --> doShift "6:email" ]
    , [className    =? "Pidgin" <&&> title =? "Buddy List" --> doShift "6:email"]
--   , [title =? "Buddy List" --> doFloatAt (1/1680) (1-400/1050) ]
--    , [className    =? "Pidgin"  --> doFloatAt (1/1680) (1-176/1050) ]
    , [className    =? c          --> doCenterFloat | c <- myFloats ]
    ])

    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
        -- classnames
        myFloats  = ["Smplayer","MPlayer","VirtualBox","Xmessage","XFontSel","Downloads","Nm-connection-editor","Eog","eog", "Galculator" ]
        myWebs    = ["Firefox","Google-chrome","Chromium", "Chromium-browser","chromium-browser"]
        myMusic   = ["Rhythmbox","Spotify"]
        myChat    = ["Pidgin", "Psi", "Psi+", "chat", "psi", "Skype"]
        --myChat          = ["Psi", "Psi+", "chat", "psi", "Skype"]
        myThunar  = ["Thunar","Gedit"]
        myDev     = ["urxvt"]
        myTmx1    = ["Tmux1","Firebug"]
        myTmx2    = ["Tmux2"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer","xfce4-notifyd"]

        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]


-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
-- }}}

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
-- myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()
myStartupHook = spawn "xset r rate 400 75"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar --screen 1 /home/luke/.xmobarrc"
  xmonad $ ewmh defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = dynamicLogWithPP $ xmobarPP
                                                  { ppOutput = hPutStrLn xmproc
                                                  , ppTitle = xmobarColor "green" "" . shorten 50
                                                  },
        startupHook        = myStartupHook
    }
