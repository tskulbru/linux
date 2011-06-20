import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces as DW
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Reflect
import XMonad.Layout.WorkspaceDir as WD
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified Data.List as L
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myHome = "/home/serrghi"
myFont = "xft:ProggyTiny:pixelsize=10"
myBgColor = "#1b1d1e"
myFgColor = "#bbbbbb"
myBorderColor = "#292c2d"
myFocusedColor = "#57666a"
--myCurrentColor = "#cd5c5c"
myCurrentColor = "#ebac54"
myEmptyColor = "#4c4c4c"
myHiddenColor = "#dddddd"
--myLayoutColor = "#666666"
myLayoutColor = "#ebac54"
myUrgentColor = "#2b9ac8"
myIcon name = myHome ++ "/.xmonad/dzen/" ++ name ++ ".xbm"

myWorkspaces = ["1:main", "2:irc", "3:web", "4:dev", "5:misc", "6:ext", "7:gfx", "8:wine"]
myTerminal = "urxvt"
myBorderWidth = 1
myModMask = mod4Mask

myStartupHook = setWMName "LG3D"

myLayoutHook = avoidStruts $
             onWorkspace "7:gfx" (Full ||| gimp) $
             onWorkspace "2:irc" (Full ||| tall) $
             onWorkspace "3:web" (Full) $
             tall ||| grid ||| Full
             where myNamed n l = named n $ layoutHints . gaps [(U, 3), (D, 3), (R, 3), (L, 3)] . spacing 3 $ l
                   grid = myNamed "grid" Grid
                   tall = myNamed "tall" (Tall 1 (3/100) (1/2))
                   gimp = withIM (0.11) (Role "gimp-toolbox") $
                          reflectHoriz $
                          withIM (0.15) (Role "gimp-dock") Full


myManageHook = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doShift  "3:web"    |   c   <- myWebs   ] -- move webs to web                                                                                                               
    , [className    =? c            --> doShift  "4:dev"    |   c   <- myDevs   ] -- move devs to dev
    , [className    =? c            --> doShift  "6:ext"    |   c   <- myExt    ] -- move external (rdesktop etc) to ext 
    , [className    =? c            --> doShift  "8:wine"   |   c   <- myWines  ] -- move wines to wine
    , [className    =? c            --> doShift  "7:gfx"    |   c   <- myGfxs   ] -- move graphicapps to gfx
    , [className    =? c            --> doFloat             |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doFloat             |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> doF W.focusDown <+> doFullFloat         ] -- YouTube fullscreen fix
    ])  

    where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        myFloats  = ["MPlayer","Zenity","VirtualBox","Xmessage","Save As...","XFontSel","Downloads","Nm-connection-editor"]
        myWebs    = ["Navigator","Shiretoko","Firefox","Uzbl","uzbl","Uzbl-core","uzbl-core","Google-chrome","Chromium","Shredder","Mail","Chrome","Thunderbird"]
        myDevs    = ["Eclipse","eclipse","Netbeans","Gvim"]
        myExt     = ["Remmina"]
        myWines   = ["Wine"]
        myGfxs    = ["Inkscape", "Gimp"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]
       -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options","gmrun"]       

myKeys conf = mkKeymap conf $
               -- kills
               [ ("M1-<F4>", kill)
               , ("M-S-q", spawn "exec killall dzen2" >> restart "xmonad" True)
               -- programs
               , ("M-p", spawn "dmenu_run -l 8 -fn \"xft:ProggyTiny-7\" -nb \"#1B1D1E\" -nf \"#a0a0a0\" -sb \"#333\" -sf \"#fff\" -p Run -b")
               , ("M-r", spawn $ XMonad.terminal conf)
               --, ("M1-<F2>", spawn "gmrun")
               , ("M-f", spawn "google-chrome")
               , ("M-t", spawn "thunderbird")
               , ("M-e", spawn "pcmanfm")
               -- layouts
               , ("M-m", windows W.shiftMaster)
               , ("M-S-t", withFocused $ windows . W.sink)
               , ("M-,", sendMessage Shrink)
               , ("M-.", sendMessage Expand)
               , ("M-l", goToSelected defaultGSConfig)
               , ("M-<Space>", sendMessage NextLayout)
               , ("M1-<Tab>", windows W.focusDown)
               -- Special keys
               , ("<XF86AudioMute>", spawn "vol mute")
               , ("<XF86AudioLowerVolume>", spawn "vol down")
               , ("<XF86AudioRaiseVolume>", spawn "vol up")
               , ("<Print>", spawn "screenshot scr")
               ]
               ++
               [ (m ++ k, f i)
                  | (i, k) <- zip ((\ws -> last ws : ws) . workspaces $ conf)
                                   ("^" : map show ([1..9] ++ [0]))
                  , (m, f) <- [ ("M-"    , windows . W.greedyView)
                              , ("M-S-"  , windows . W.shift)
                              ]
               ]


myStatusBar = "dzen2 -x 0 -y 0 -h 16 -w 640 -ta l -fn " ++ myFont ++ " -bg \"" ++ myBgColor ++ "\" -fg \"" ++ myFgColor ++ "\""
myResourceBar = "MY_HOME=\"" ++ myHome ++ "\" conky -c ~/.xmonad/conky.conf | dzen2 -x 640 -y 0 -h 16 -w 640 -ta r -fn " ++ myFont ++ " -bg \"" ++ myBgColor ++ "\" -fg \"" ++ myFgColor ++ "\""
myLogHook h = dynamicLogWithPP $ defaultPP
                                  { ppOutput = hPutStrLn h
                                  , ppCurrent = corner . fg myCurrentColor
                                  , ppHidden = corner . fg myHiddenColor
                                  , ppHiddenNoWindows = corner . fg myEmptyColor
                                  , ppUrgent = corner . fg myUrgentColor . dzenStrip
                                  , ppLayout = fg myLayoutColor . layout
                                  , ppWsSep = "  "
                                  , ppSep = "     "
                                  , ppTitle = fg myFgColor . dzenEscape . shorten 100 . trim
                                  }
            where fg c = dzenColor c ""
                  icon n = "^i(" ++ (myIcon n) ++ ")"
                  corner = (++) (icon "corner")
                  layout n = icon ("layout-" ++ n)

main = do
        status <- spawnPipe myStatusBar
        resource <- spawnPipe myResourceBar
        spawn "sh /home/serrghi/.xmonad/autostart.sh"
        xmonad $ withUrgencyHook NoUrgencyHook
               $ defaultConfig
                  { workspaces = myWorkspaces
                  , terminal = myTerminal
                  , borderWidth = myBorderWidth
                  , modMask = myModMask
                  , normalBorderColor = myBorderColor
                  , focusedBorderColor = myFocusedColor
                  , keys = myKeys
                  , layoutHook = myLayoutHook
                  , logHook = myLogHook status
                  , startupHook = myStartupHook
                  , manageHook = myManageHook
                  }
