-- Imports {{{
import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
-- Hooks
import XMonad.Operations

import System.IO
import System.Exit

import XMonad.Util.Run


import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps

import qualified XMonad.StackSet as W
import qualified Data.Map as M

--}}}

-- Config {{{
-- Define Terminal
myTerminal      = "urxvt"
-- Define modMask
modMask' :: KeyMask
modMask' = mod4Mask
-- Define workspaces
myWorkspaces    = ["1:main","2:irc","3:web","4:dev","5:foo()","6:wine"]
-- Dzen config
myStatusBar = "dzen2 -x '0' -y '0' -h '24' -w '1280' -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
myBtmStatusBar = "conky -c /home/serrghi/.conky_bottom_dzen | dzen2 -x '0' -w '1280' -h '24' -ta 'c' -bg '#161616' -fg '#FFFFFF' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*' -y -1"
myBitmapsDir = "/home/serrghi/.xmonad/dzen"
--}}}
-- Main {{{
main = do
    dzenTopBar <- spawnPipe myStatusBar
    dzenBtmBar <- spawnPipe myBtmStatusBar
    spawn "sh /home/serrghi/.xmonad/autostart.sh"
    xmonad $ defaultConfig
      { terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = keys'
      , modMask             = modMask'
      , startupHook         = ewmhDesktopsStartup >> setWMName "LG3D"
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , logHook             = myLogHook dzenTopBar >> fadeInactiveLogHook 0xdddddddd  >> setWMName "LG3D"
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
}
--}}}


-- Hooks {{{
-- ManageHook {{{
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doShift  "3:web"    |   c   <- myWebs   ] -- move webs to webs
    , [className    =? c            --> doShift  "4:dev"    |   c   <- myDevs   ] -- move devs to devs
    , [className    =? c            --> doF(W.shift "6:wine")   |   c   <- myWines  ] -- move wines to wine
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    ]) 

    where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        myFloats  = ["MPlayer","Zenity","VirtualBox","Xmessage","Save As...","XFontSel","Downloads","Nm-connection-editor"]
        myWebs    = ["Navigator","Shiretoko","Firefox","Uzbl","uzbl","Uzbl-core","uzbl-core","Google-chrome","Chromium","Shredder","Mail"]
        myDevs    = ["Eclipse","eclipse","Netbeans","Gvim"]
        myWines   = ["Wine"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]

        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
-- }}}
layoutHook' = customLayout

-- Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#161616" . pad
      , ppVisible           =   dzenColor "white" "#161616" . pad
      , ppHidden            =   dzenColor "white" "#161616" . pad
      , ppHiddenNoWindows   =   dzenColor "#444444" "#161616" . pad
      , ppUrgent            =   dzenColor "red" "#161616" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#ebac54" "#161616" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#161616" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
-- Layout
customLayout = gaps [(D,16)] $ avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled)  ||| noBorders Full ||| smartBorders simpleFloat
  where
    --tiled = ResizableTall 1 (2/100) (1/2) []
    tiled   = ResizableTall nmaster delta ratio []
    nmaster = 1   
    delta   = 2/100
    ratio   = 1/2
--}}}
-- Theme {{{
-- Color names are easier to remember:
colorOrange          = "#ff7701"
colorDarkGray        = "#171717"
colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#fee100"
colorWhite           = "#cfbfad"
 
colorNormalBorder    = "#1c2636"
colorFocusedBorder   = "#ebac54"
barFont  = "terminus"
barXFont = "inconsolata:size=14"
xftFont = "xft: inconsolata-14"
--}}}

-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 14
                    , historyFilter         = deleteConsecutive
                    }
 
-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 20
                }
-- }}}
-- Key mapping {{{
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_p        ), runOrRaisePrompt largeXPConfig)
    , ((mod1Mask,                   xK_F2       ), spawn "gmrun")
    , ((0,                          xK_Print    ), spawn "screenshot scr")

    -- Programs
    , ((modMask,                    xK_r        ), spawn $ XMonad.terminal conf) -- spawn terminal
    , ((modMask,                    xK_f        ), spawn "google-chrome")
    , ((modMask,                    xK_t        ), spawn "thunderbird")
    , ((modMask,                    xK_e        ), spawn "pcmanfm")

    -- Media Keys
    , ((0,                          0x1008ff12  ), spawn "vol mute") -- XF86AudioMute
    , ((0,                          0x1008ff11  ), spawn "vol down") -- XF86AudioLowerVolume
    , ((0,                          0x1008ff13  ), spawn "vol up") -- XF86AudioRaiseVolume

    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf) -- reset layout on current desktop to default
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts)
    , ((mod1Mask,                   xK_Tab      ), windows W.focusDown) -- move focus to next window
    , ((mod1Mask,                   xK_F4       ), kill) -- kill selected window
    , ((modMask .|. shiftMask,      xK_j        ), windows W.swapDown) -- swap the focused window with the next window
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp)  -- swap the focused window with the previous window
    , ((modMask .|. shiftMask,      xK_t        ), withFocused $ windows . W.sink) -- Push window back into tiling

    -- workspaces
    , ((mod1Mask .|. controlMask,   xK_Right    ), nextWS)
    , ((mod1Mask .|. shiftMask,     xK_Right    ), shiftToNext)
    , ((mod1Mask .|. controlMask,   xK_Left     ), prevWS)
    , ((mod1Mask .|. shiftMask,     xK_Left     ), shiftToPrev)
    
    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask              ,      xK_q        ), restart "xmonad" True)
    , ((modMask .|. shiftMask,      xK_r        ), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

--}}}
-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap 
