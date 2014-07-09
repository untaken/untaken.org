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
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Layout.Minimize
import XMonad.Actions.Search

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Ratio ((%))
-- }}}

-- Set Terminal; Border colour/width; modmask; workspace names {{{
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvtc +bc +uc -cr Green"

-- Width of the window border in pixels.
--
myBorderWidth   = 2

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
myWorkspaces    = ["1:tmux","2:tmux","3:web","4:email","5:misc","6:vbox"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#A13322"
myFocusedBorderColor = "#00afd7"

-- }}}

-- Key bindings. Add, modify or remove key bindings here. {{{
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- Shortcut to restart services
    , ((0, xK_Super_L      ), spawn "~/bin/restart_services.sh 2> /dev/null")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm, xK_Escape      ), kill)

    , ((modm, xK_Super_L ), spawn "xmenud.py")

    , ((0, xK_Alt_R      ), windows $ viewOnScreen 0 "1:tmux" . viewOnScreen 1 "2:tmux")
    , ((0, xK_Super_R      ), windows $ viewOnScreen 0 "1:tmux" . viewOnScreen 1 "3:web")
    , ((0, xK_Control_R      ), windows $ viewOnScreen 0 "4:email" . viewOnScreen 1 "1:tmux")
    , ((0, xK_Menu      ), windows $ viewOnScreen 0 "4:email" . viewOnScreen 1 "5:spotify")
    , ((modm, xK_Menu      ), windows $ viewOnScreen 0 "4:email" . viewOnScreen 1 "1:tmux")
    , ((modm, xK_Control_R      ), windows $ viewOnScreen 0 "5:spotify" . viewOnScreen 1 "6:vbox")

    , ((modm, xK_KP_Insert ), namedScratchpadAction scratchpads "urxvtc1")
    , ((modm, xK_KP_Delete ), namedScratchpadAction scratchpads "urxvtc2")
    , ((modm , xK_KP_End ), scratchpadSpawnActionTerminal "urxvt")
    , ((modm, xK_KP_Page_Down ), namedScratchpadAction scratchpads "gedit")
    , ((modm, xK_KP_Down ), namedScratchpadAction scratchpads "gedit" <+> namedScratchpadAction scratchpads "thunar")
    , ((modm, xK_KP_Left ), namedScratchpadAction scratchpads "spotify")
    , ((modm, xK_KP_Right ), allNamedScratchpadAction scratchpads "xpad")

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
    -- , ((modm,               xK_m     ), windows W.focusMaster  )

    , ((modm,               xK_m     ), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)

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

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    , ((modm .|. shiftMask, xK_Delete), spawn "gksudo '/usr/lib/indicator-session/gtk-logout-helper --shutdown'")

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
    , ((modm,               xK_z),     toggleWS' ["NSP"])
    , ((modm,               xK_Alt_R),     swapNextScreen)

    , ((modm, xK_c),    spawn "tmux save-buffer - | xclip -i -selection clipboard")
    , ((modm, xK_v),    spawn "tmux set-buffer -- \"$(xclip -o -selection clipboard)\"; tmux paste-buffer")

    -- , ((modm .|. shiftMask, xK_l),    spawn "xscreensaver-command -lock")
    , ((modm .|. shiftMask, xK_l),    spawn "gnome-screensaver-command --lock")

    -- Spotify controls for pause, skip and previous
    , ((0, xK_Pause),    spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ((0 .|. shiftMask, xK_Print),    spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    , ((0 .|. shiftMask, xK_Scroll_Lock),    spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")

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

myLayout  =  spacing 7                                                          $
             onWorkspaces ["1:tmux", "3:web", "5:spotify", "6:vbox" ]  allLayout $
             onWorkspaces ["2:tmux", "4:email"]                        tallLayout $
             -- onWorkspaces ["4:thunar"]                                 thunarLayout $
             allLayout
-- Layout
tallLayout = avoidStruts $ minimize Full ||| minimize tiled 
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

allLayout = avoidStruts $ minimize Full ||| minimize tiled ||| minimize (Mirror tiled)
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

vboxlayout =  avoidStruts $ Full

-- }}}

-- Window rules; Shifting them to correct place {{{

myManageHook = (composeAll . concat $
    [ [resource     =? r          --> doIgnore             |   r   <- myIgnores]
    , [title        =? "Tmux1"    --> doShift  "1:tmux"]
    , [appName      =? "Firebug"  --> doShift  "1:tmux"]
    , [appName      =? "Global"    --> doShift  "1:tmux"]
    , [title        =? "Tmux2"    --> doShift  "2:tmux"] 
    , [className    =? c          --> doShift  "3:web"     |   c   <- myWebs   ]
    , [className    =? c          --> doShift  "5:spotify" |   c   <- myMusic  ]
    , [className    =?           "Orage" --> doFloatAt (1/1680) (1-176/1050) ]
    , [className    =?           "Thunderbird" --> doShift "4:email" ]
    , [className    =?           "Okular" --> doShift "5:misc" ]
    , [ prefixTitle "libreoffice" <||> prefixTitle "LibreOffice" --> doShift "5:misc" ] 
    , [className    =?           "VirtualBox" --> doShift "6:vbox" ]
    , [className    =? "Pidgin" <&&> title =? "Buddy List" --> doShift "4:email"]
    , [className    =? c          --> doCenterFloat | c <- myFloats ]
    ])

    where
        prefixTitle prefix = fmap (prefix `isPrefixOf`) title
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
        -- classnames
        myFloats  = ["Smplayer","MPlayer","Xmessage","VirtualBox","XFontSel","Downloads","Nm-connection-editor","Eog","eog", "Galculator","xpad" ]
        myWebs    = ["Firefox","Google-chrome","Chromium", "Chromium-browser","chromium-browser"]
        myMusic   = ["Rhythmbox","Spotify"]
        myChat    = ["Pidgin", "Psi", "Psi+", "chat", "psi", "Skype"]
        -- myThunar  = ["Thunar","Gedit"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer","xfce4-notifyd"]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
-- }}}

-- Scratchpads {{{
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)

  where

    h = 0.4     -- terminal height, 10%
    w = 1       -- terminal width, 100%
    t = 1 - h   -- distance from top edge, 90%
    l = 1 - w   -- distance from left edge, 0%


-- Named scratchpad
scratchpads = [
 -- run htop in xterm, find it by title, use default floating window placement
     NS "thunar" "thunar" (className =? "Thunar") 
     (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6)),

     NS "gedit" "gedit" (className =? "Gedit")
     (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6)), 

     NS "spotify" "spotify" (className =? "Spotify")
     (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6)), 

     NS "urxvtc1" "urxvtc -name urxvtc1 -pe tabbed -bg '#000033'" (title =? "urxvtc1")
     (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6)), 

     NS "urxvtc2" "urxvtc -name urxvtc2 -pe tabbed -bg '#330000'" (title =? "urxvtc2")
     (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6)),

     NS "xpad" "xpad" (className =? "xpad")
     (defaultFloating) 

 ] where role = stringProperty "WM_WINDOW_ROLE"
-- }}}


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
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc_2nd"
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
        manageHook         = myManageHook <+> manageScratchPad <+> namedScratchpadManageHook scratchpads <+> manageDocks,
        logHook            = dynamicLogWithPP xmobarPP
                                                  { ppOutput = hPutStrLn xmproc
                                                  , ppTitle = xmobarColor "#5fd7d7" "" . shorten 50
                                                  },
        startupHook        = myStartupHook
    }
