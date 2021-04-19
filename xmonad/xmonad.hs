-- Base
import XMonad
-- Hooks
import XMonad.Hooks.DynamicLog (defaultPP, dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ManageDocks (docks, avoidStruts, avoidStrutsOn, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ServerMode
-- Data
import Data.Monoid
import Data.Maybe (fromJust)
-- System
import System.Exit
-- Utils
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP)
-- Qualifieds
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
-- Layouts
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing


myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myEmojiFont :: String
myEmojiFont = "xft:JoyPixels:regular:size=9:antialias=true:hinting=true"

myTerminal           :: String
myTerminal           = "kitty"

myBrowser            :: String
myBrowser            = "firefox"

myEditor             :: String
myEditor             = "emacsclient -c -a emacs"

myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True

myClickJustFocuses   :: Bool
myClickJustFocuses   = False


myBorderWidth        = 1

myModMask            = mod4Mask

myWorkspaces         = ["dev","learning","cfgs","chitchat","testing","vlc","gaming, maybe","irc","idk"]
myWorkspaceIndices   = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)
clickable ws         = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i          = fromJust $ M.lookup ws myWorkspaceIndices


--ppTitle             :: String
--ppTitle              = xmobarColor "#ddcbbf" ""


--
myNormalBorderColor  = "#52BAB7"
myFocusedBorderColor = "#38a4a1"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- Not using DMenu nor GM_Run. ROFI FTW
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm, xK_space ),               sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm, xK_n     ),               refresh)
    , ((modm, xK_Tab   ),               windows W.focusDown)
    , ((modm, xK_j     ),               windows W.focusDown)
    , ((modm, xK_k     ),               windows W.focusUp  )
    , ((modm, xK_m     ),               windows W.focusMaster  )
    , ((modm, xK_Return),               windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j),      windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k),      windows W.swapUp    )
    , ((modm, xK_h     ),               sendMessage Shrink)
    , ((modm, xK_l     ),               sendMessage Expand)
    , ((modm, xK_t     ),               withFocused $ windows . W.sink)
    , ((modm, xK_comma ),               sendMessage (IncMasterN 1))
    , ((modm, xK_period),               sendMessage (IncMasterN (-1)))
    -- ToggleStruts, KillXMonad, RestartXMonad. TSS
    , ((modm .|. shiftMask, xK_s),      sendMessage ToggleStruts)
    , ((modm  .|. shiftMask, xK_q),     io (exitWith ExitSuccess))
    , ((modm, xK_q),                    spawn "xmonad --recompile; xmonad --restart")
    -- Some of my commands.
    -- Controls the brightness of my screen.
    , ((modm, xK_p),                    spawn "rofi -show run")
    , ((modm .|. shiftMask, xK_p),      spawn "rofi -show drun")
    , ((modm, xK_F5),                   spawn "xbacklight -dec 10")
    , ((modm, xK_F6),                   spawn "xbacklight -inc 10")
    -- Controls the Volume.
    , ((modm, xK_F10),                  spawn "amixer set Master 0%")
    , ((0,    xK_F10),                  spawn "amixer set Master 100%")
    , ((0,    xK_F11),                  spawn "amixer set Master 5%-")
    , ((0,    xK_F12),                  spawn "amixer set Master 5%+")
    -- Opens myBrowser
    , ((modm, xK_f),                    spawn myBrowser)
    , ((0,    xK_Print),                spawn "spectacle -g")
    , ((modm, xK_Return),               spawn myEditor)
    , ((modm .|. shiftMask, xK_d),      spawn "rofi -show file-browser")
    , ((modm .|. shiftMask, xK_m),      spawn "keepass")
    , ((modm, xK_z),                    spawn "zim")
    , ((modm, xK_d),                    spawn "discord")
    , ((modm .|. shiftMask, xK_f),      spawn "alacritty")
    , ((modm, xK_a),                    spawn "pavucontrol")
    , ((modm .|. shiftMask, xK_n),      spawn "manviewer")
    ]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myBinds :: [(String, X ())]
myBinds =
    [ --("M-m i", spawn "firefox")
      ("M-s w", spawn "kitty")
    , ("M-s z", spawn "mpc volume")
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full ||| noBorders Full)
  where
     tiled   = spacing 5 $ Tall nmaster delta ratio

     nmaster = 1

     ratio   = 1/2

     delta   = 3/100

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Steam"          --> doShift                            ( myWorkspaces !! 2 )
    , className =? "steam_app"      --> doFloat
    , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat 
    , className =? "Gimp"           --> doFloat
    , className =? "vlc"            --> doShift                            ( myWorkspaces !! 1 )
    , className =? "obs"            --> doShift                            ( myWorkspaces !! 1 )
    , className =? "midori"         --> doShift                            ( myWorkspaces !! 1 )
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , className =? "Hacknet"        --> doFloat 
    , className =? "Spectacle"      --> doFloat
    , resource  =? "./PapersPlease" --> doFloat]

myEventHook = mempty

myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 1.0

myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"
  spawnOnce "emacs --daemon"
  spawnOnce "lxsession &"
  spawnOnce "setxkbmap latam"

main :: IO ()
main = do
  h <- spawnPipe "xmobar /home/b0ss/.config/xmobar/xmobarrc2"
  --h <- spawnPipe "/home/b0ss/.config/polybar/launch.sh --forest"
  xmonad $ docks $ ewmh $ def
        { terminal               = myTerminal
        , focusFollowsMouse      = myFocusFollowsMouse
        , clickJustFocuses       = myClickJustFocuses
        , borderWidth            = myBorderWidth
        , workspaces             = myWorkspaces
        , normalBorderColor      = myNormalBorderColor
        , focusedBorderColor     = myFocusedBorderColor
      -- key bindings
        , modMask                = myModMask
        , keys                   = myKeys
        , mouseBindings          = myMouseBindings
      -- hooks, layouts
        , layoutHook             = myLayout
      --manageHook               = myManageHook,
        , manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        , handleEventHook        = myEventHook
        , logHook                = myLogHook <+> dynamicLogWithPP xmobarPP
                                 { ppOutput = hPutStrLn h 
    	}
        , startupHook            = myStartupHook
    }  `additionalKeysP` myBinds
