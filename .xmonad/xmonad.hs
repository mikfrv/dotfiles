
----- Mikael's XMonad Config -----


------------------------------------------------------------------------
-- //  IMPORTS
------------------------------------------------------------------------

--- Base ---
import XMonad
import qualified XMonad.StackSet as W
import System.Exit

--- Data ---
import Data.Monoid
import qualified Data.Map        as M
import Data.Char (toLower)

--- Hooks ---
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

--- Layouts ---
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances

--- Utilities ---
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Hacks as Hacks

--- Color Schemes ---
import Colors.SolarizedDark


------------------------------------------------------------------------
-- //  Config
------------------------------------------------------------------------

main = xmonad
     . docks
     . ewmh
     . withSB (statusBarProp "xmobar" (pure myXmobarPP))
     $ myDesktopConfig `additionalKeysP` myKeymaps

-- My Desktop Overrides
myDesktopConfig = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormColor,
        focusedBorderColor = myFocusColor,

      -- key bindings
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

------------------------------------------------------------------------
-- //  Variables
------------------------------------------------------------------------
 
myTerminal :: String
myTerminal = "alacritty"  -- Sets the preferred terminal

myBrowser :: String
myBrowser = "brave"

myModMask :: KeyMask 
myModMask = mod4Mask     -- Super_L

myBorderWidth :: Dimension
myBorderWidth = 2   -- Pixel width of the window border

myNormColor :: String
myNormColor = colorBG -- Border colour for normal windows

myFocusColor :: String
myFocusColor = color15 -- Border Colour for focused windows

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True -- Whether focus follows the mouse pointer.

myClickJustFocuses :: Bool
myClickJustFocuses = False -- Whether clicking on a window to focus also passes the click to the window

-- Workspaces can be set to any string for naming purposes.
myWorkspaces    = ["main","web","dev","doc","comm","game","misc","virt","sys"]

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i  = spacingRaw False (Border i i i i) True (Border i i i i) True


------------------------------------------------------------------------
-- //  Layouts
------------------------------------------------------------------------

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts
	 $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
         $ tall ||| wide ||| threeCol ||| full

tall     = renamed [Replace "tall"]
         $ smartBorders
	 $ mySpacing 6
         $ Tall 1 (3/100) (1/2)

wide     = renamed [Replace "wide"]
         $ smartBorders
	 $ mySpacing 6
         $ Mirror $ Tall 1 (3/100) (1/2)

threeCol = renamed [Replace "mid column"]
         $ smartBorders
	 $ mySpacing 6
	 $ ThreeColMid 1 (3/100) (1/2)

full     = renamed [Replace "full"]
         $ smartBorders
	 $ mySpacing 6
         $ Full

------------------------------------------------------------------------
-- // Window rules
------------------------------------------------------------------------

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
myManageHook = composeAll
    [ className =? "Gimp"               --> doFloat
    , title     =? "Picture-in-Picture" --> doFloat
    , className =? "Zotero" <&&> title =? "Zotero Preferences" --> doFloat
    , className =? "Steam"  <&&> title =? "Friends List"       --> doFloat ]

------------------------------------------------------------------------
-- //  Event handling
------------------------------------------------------------------------

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- windowedFullscreenFixEventHook fixes the problem of chromium-based
-- browsers not respecting the allotted window space in fullscreen video.

myEventHook = handleEventHook def <+> Hacks.windowedFullscreenFixEventHook

------------------------------------------------------------------------
-- //  Status bars and logging
------------------------------------------------------------------------

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

myXmobarPP :: PP
myXmobarPP = xmobarPP
  { ppCurrent         = xmobarColor color10 "" . wrap
	                ("<box type=Bottom width=2 mb=2 color=" ++ color10 ++ ">") "</box>"
  , ppVisible         = xmobarColor color07 ""
  , ppHidden          = xmobarColor colorFG "" . wrap
			("<box type=Bottom width=2 mb=2 color=" ++ colorFG ++ ">") "</box>"
  , ppHiddenNoWindows = xmobarColor colorFG ""
  , ppTitle           = xmobarColor colorFG "" . shorten 30 . map toLower
  , ppLayout          = map toLower
  , ppSep             = "<fc=" ++ color11 ++ "> <fn=1>  |  </fn> </fc>"
  , ppWsSep           = xmobarColor color11 "" "     "
  , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
}
------------------------------------------------------------------------
-- //  Startup hook
------------------------------------------------------------------------

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
	spawnOnce "nitrogen --restore &"
	spawnOnce "picom &"
	spawnOnce "volnoti &"
	spawnOnce "xbindkeys &"
------------------------------------------------------------------------
-- //  Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------

myKeymaps = 
    [
      -- Toggle on fullscreen with no borders
      ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
    ]

------------------------------------------------------------------------
-- //  Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]



-----------------------------------------------------------------------
-- //  Default Keybindings
-----------------------------------------------------------------------

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
