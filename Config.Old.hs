import XMonad

import XMonad.Actions.WithAll(killAll)
import XMonad.Actions.CycleWS(toggleWS,toggleOrView)
import XMonad.Actions.DynamicWorkspaces (addWorkspace)
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import XMonad.Actions.SwapPromote

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Renamed(renamed, Rename(Replace))
import XMonad.Layout.Spacing (spacing)

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BoringWindows(boringWindows)
import qualified XMonad.Layout.BoringWindows as BW(focusUp, focusDown)
import XMonad.Layout.GridVariants(Grid (Grid))
import XMonad.Layout.Hidden
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPanePersistent
import XMonad.Layout.WindowNavigation

-- import XMonad.Layout.Groups.Wmii

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.NamedScratchpad

import System.Exit
import System.IO

import qualified XMonad.StackSet as W

getActiveLayoutDescription :: X String
getActiveLayoutDescription = do
    workspaces <- gets windowset
    return $ description . W.layout . W.workspace . W.current $ workspaces

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ ewmh $ withNavigation2DConfig myNav2DConf $ def
    { terminal              = myTerminal
    , manageHook            = myManageHook <+> manageHook def
    , layoutHook            = myLayoutHook
    , startupHook           = myStartupHook
    , logHook               = dynamicLogWithPP xmobarPP
                             { ppOutput = hPutStrLn xmproc
                            -- , ppHidden = xmobarColor myNormalText "" . pad
                            -- , ppTitle = xmobarColor "#90b3db" "" . shorten 50
                            -- , ppCurrent = xmobarColor "#e9e9e9" "#5e6964" . pad
                            -- #d2de87 #90a1ad #eaa2a6
                             , ppCurrent = xmobarColor "#ffffff" "#005577" . wrap " " " " -- Current workspace in xmobar
                             , ppVisible = xmobarColor "#ffffff" "" . pad                -- Visible but not current workspace
                             , ppHidden = xmobarColor "#ffffff" "" . wrap " *" " "   -- Hidden workspaces in xmobar
                             , ppHiddenNoWindows = xmobarColor "#999999" "" . pad        -- Hidden workspaces (no windows)
                             , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60
                             , ppSep = " <fc=#666666> | </fc> "
                             , ppExtras = [windowCount]
                             , ppSort = fmap (namedScratchpadFilterOutWorkspace.) (ppSort defaultPP)
                             } >> masterHistoryHook
    , handleEventHook       = handleEventHook def <+> fullscreenEventHook
    , modMask               = myModMask
    , borderWidth           = myBorderWidth
    , normalBorderColor     = "#46423a"
    , focusedBorderColor    = "#285577"
    --, focusedBorderColor    = "#e6ac32"
    , workspaces            = myWorkspaces
    } `additionalKeysP` myKeys

myNormalBackground          = "#2c2b29"
myNormalText                = "#e9e9e9"
myNormalBorder              = "#4c4c4c"
myFocusedBackground         = "#90b3db"
myFocusedText               = "#2c2b29"
myFocusedBorder             = "#90b3db"

myTerminal                  = "xst"
myModMask                   = mod4Mask
myBorderWidth               = 2

myTabConfig = def { activeColor         = myFocusedBackground
                  , activeBorderColor   = myFocusedBorder
                  , activeTextColor     = myFocusedText
                  , inactiveColor       = myNormalBackground
                  , inactiveBorderColor = myNormalBorder
                  , inactiveTextColor   = myNormalText
                  , fontName            = "xft:Ubuntu Mono:size=8:antialias=true"
                  , decoHeight          = 40
                  }

myColorizer = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x61,0x57,0x72) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg

myGridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 70
    , gs_cellwidth    = 350
    , gs_cellpadding  = 8
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = "xft:Ubuntu Mono:size=8:antialias=true"
    }

myWorkspaces                = ["term", "web", "file", "work"] ++ map show [5..9] ++ [ "X" ]
myAdditionalWorkspaces      = [(xK_x, "X")]

scratchpads = [ NS "scratchTerm" "xst -n scratchpad" (resource =? "scratchpad") (customFloating $ W.RationalRect 0.2 0.2 0.6 0.6)
              , NS "new" "nemo ~/ext/new" (title =? "new") (customFloating $ W.RationalRect 0.3 0.3 0.4 0.4)
              ]

myLayoutHook = avoidStruts 
                $ configurableNavigation noNavigateBorders 
                $ onWorkspace "X" grid
                $ onWorkspace "work" flexWide
                $ flex ||| tcm ||| flexWide ||| mc ||| tall ||| grid ||| bsp
    where
        tcm     = renamed [Replace "flex 3col"]
                    $ smartBorders
                    $ boringWindows
                    -- $ spacing 12
                    $ addTabs shrinkText myTabConfig
                    $ subLayout [] Simplest
                    $ ThreeColMid 1 (3/100) (1/2)
        mc      = renamed [Replace "mc"] 
                    $ smartBorders
                    $ boringWindows
                    $ addTabs shrinkText myTabConfig
                    $ subLayout [] Simplest
                    $ multiCol [1,2] 0 (3/100) (2/5)
        twopane = renamed [Replace "twopane"] $ spacing 12 $ TwoPanePersistent Nothing (3/100) golden
        tall    = renamed [Replace "tall"] $ spacing 6 $ ResizableTall 1 (2/100) (11/20) []
        grid    = renamed [Replace "grid"] $ boringWindows $ hiddenWindows $ spacing 6 $ Grid (16/10)
        bsp     = renamed [Replace "bsp"] $ spacing 6 $ emptyBSP
        flex    = renamed [Replace "flex"]
                    $ smartBorders
                    $ boringWindows
                    $ hiddenWindows
                    $ addTabs shrinkText myTabConfig
                    $ subLayout [] Simplest
                    $ ResizableTall 1 (1/20) (1/2) []
        flexWide = renamed [Replace "flex wide"]
                    $ smartBorders
                    $ boringWindows
                    $ addTabs shrinkText myTabConfig
                    $ subLayout [] Simplest
                    $ ResizableTall 1 (1/20) (2/3) []
        golden = toRational $ 2 / (1 + sqrt 5 :: Double)

myNav2DConf = def
    { defaultTiledNavigation = centerNavigation
    , floatNavigation = centerNavigation
    , screenNavigation = lineNavigation
    }

myGridWorkspace = gridselectWorkspace' defaultGSConfig
                    { gs_navigate = navNSearch
                    , gs_rearranger = searchStringRearrangerGenerator id
                    }
                addWorkspace

myKeys =
    [ ("M-<Return>", spawn myTerminal) --whenX (swapHybrid True) dwmpromote)
    , ("M-<Backspace>", windows $ W.swapMaster)
    , ("M-n",       spawn "nemo")
    , ("M-S-n",     namedScratchpadAction scratchpads "new")
    , ("M-d",       spawn "/home/rkb/bin/appmenu")
    , ("M-S-d",       spawn "/home/rkb/bin/windowmenu")
    , ("M-S-p",     myGridWorkspace)
    , ("M-S-e",     spawn $ myTerminal ++ " -e nvim ~/.xmonad/xmonad.hs")
    , ("M-c",       kill)
    , ("M-b",       spawn "chromium")
    , ("M-f",       goToSelected defaultGSConfig)
    , ("M-g",       withFocused (sendMessage . MergeAll))
    , ("M-S-g",     withFocused (sendMessage . UnMerge))
    , ("M-u",       sendMessage MirrorShrink)
    , ("M-i",       sendMessage MirrorExpand)
    , ("M-<Tab>",   toggleWS)
    , ("M-S-c",     killAll)
    , ("M-S-q",     spawn "killall xmobar; xmonad --recompile && xmonad --restart")
    , ("M-S-x",       toggleOrView "X")
    --, ("M-S-x",     windows $ W.shift "X")
    , ("M-M1-q",    io (exitWith ExitSuccess))
    , ("M-j",       BW.focusDown)
    , ("M-k",       BW.focusUp)
    , ("M-C-j",     withFocused (sendMessage . mergeDir W.focusDown'))
    , ("M-C-k",     withFocused (sendMessage . mergeDir W.focusUp'))
    , ("M-;",       onGroup W.focusUp')
    , ("M-'",       onGroup W.focusDown')
--    , ("M-h",       windowGo L False)
--    , ("M-j",       windowGo D False)
--    , ("M-k",       windowGo U False)
--    , ("M-l",       windowGo R False)
--    , ("M-S-h",     windowSwap L False)
--    , ("M-S-j",     windowSwap D False)
--    , ("M-S-k",     windowSwap U False)
--    , ("M-S-l",     windowSwap R False)
--    , ("M-w",       sendMessage $ Go U)
--    , ("M-a",       sendMessage $ Go L)
--    , ("M-s",       sendMessage $ Go D)
--    , ("M-d",       sendMessage $ Go R)
    , ("M-<U>",     sendMessage $ Go U)
    , ("M-<L>",     sendMessage $ Go L)
    , ("M-<D>",     sendMessage $ Go D)
    , ("M-<R>",     sendMessage $ Go R)
    , ("M-S-<U>",     sendMessage $ pullGroup U)
    , ("M-S-<L>",     sendMessage $ pullGroup L)
    , ("M-S-<D>",     sendMessage $ pullGroup D)
    , ("M-S-<R>",     sendMessage $ pullGroup R)
    , ("M--",       namedScratchpadAction scratchpads "scratchTerm")
    , ("M-=",       spawn myTerminal)
    , ("M-y",       withFocused hideWindow)
    , ("M-S-y",     popNewestHiddenWindow)
    , ("M-x f",     spawn "firefox")
    ]

-- myStartupHook = do
  -- spawn "mkfifo /tmp/xmonad-workspace-log"
  -- spawn "$HOME/.config/polybar/launch.sh"
--   spawn "picom --config /home/rkb/.config/picom/picom.conf"

myStartupHook = do
    spawn "i3status -c ~/.config/i3status/config-xmobar | xmobar ~/.xmobarrc2"

myManageHook = composeAll
--   [ className =? "mpv"         --> doFloat
   [ className =? "Chromium"    --> doShift "web"
   , className =? "Firefox"     --> doShift "web"
   , manageDocks
   ] <+> namedScratchpadManageHook scratchpads

