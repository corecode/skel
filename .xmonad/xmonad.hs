{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}

-- derived from XMonad configuration file by Thomas ten Cate <ttencate@gmail.com>
--
-- Works on xmonad-0.8, NOT on 0.7 or below; and of course
-- xmonad-contrib needs to be installed as well
--
-- This is designed to play nice with a standard Ubuntu Hardy installation.
-- It gives you 10 workspaces, available through Alt+F1..F10. You can move
-- windows to a workspace with Win+F1..F10 (and you will automatically switch
-- to that workspace too).
--
-- All workspaces except F9 respect panels and docks.
-- F9 is the fullscreen workspace (for mplayer, etc.).
-- F10 is the instant messaging workspace.
--
-- Pidgin and Skype windows are automatically placed onto the IM workspace.
-- Their contact lists each get a column on the right side of the screen,
-- and all their other windows (chat windows, etc.) go into a grid layout
-- in the remaining space.
-- (This uses a copied and modified version of XMonad.Layout.IM.)
--
-- Keybindings mostly use the Windows key, but some use Alt to mimic other
-- window managers. In general: Alt is used for navigation, Win for modification.
-- Some of the bindings resemble the XMonad defaults, but most don't.
-- The bindings are set up to be comfortable to use on a dvorak keyboard layout.
--
-- Navigation:
-- Alt/Win+F1..F10      switch to workspace
-- Shift+Alt+Left/Right switch to previous/next workspace
-- Alt+Tab              focus next window
-- Alt+Shift+Tab        focus previous window
-- Alt/Win+W/E/R        focus 1st/2nd/3rd Xinerama screen
--
-- Window management:
-- Control+Alt/Win+F1..F10 move window to workspace
-- Control+Win+W/E/R    move window to screen
-- Win+Up/Down          move window up/down
-- Win+C                close window
-- Alt+ScrollUp/Down    move focused window up/down
-- Win+M                move window to master area
-- Win+N                refresh the current window
-- Win+LMB              move floating window
-- Win+RMB              resize floating window
-- Shift+Win+LMB        unfloat floating window
-- Win+T                unfloat floating window
--
-- Layout management:
-- Win+Left/Right       shrink/expand master area
-- Win+B/V              move more/less windows into master area
-- Win+Space            cycle layouts
--
-- Other:
-- Win+Enter            start a terminal
-- Win+P                dmenu
-- Shift+Win+P          gmrun
-- Shift+Win+E          run "e" == emacsclient
-- Ctrl+Win+Q           restart XMonad
-- Shift+Ctrl+Win+Q     quit session
-- Ctrl+Alt+L           lock session

import XMonad
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as S
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.UpdatePointer
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Util.WindowProperties
-- import XMonad.Util.Run(spawnPipe)
import System.Exit
import Control.Monad
import Data.Ratio
import qualified Data.Map as M

-- defaults on which we build
-- use e.g. defaultConfig or gnomeConfig
myBaseConfig = ewmh defaultConfig

myTerminal = "urxvt -ls"

-- display
-- replace the bright red border with a more stylish colour
myBorderWidth = 2
myNormalBorderColor = "#505060"
myFocusedBorderColor = "#FF3030"

-- workspaces
-- myWorkspaces = ["dev", "work", "web", "mail"] ++ (miscs 4) ++ ["fs", "im"]
--     where miscs = map (show) . (flip take) [1..]
myWorkspaces = (count 10)
    where count = map (show) . (flip take) [1..]

-- layouts
basicLayout = Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
tallLayout = named "tall" $ avoidStruts $ basicLayout
wideLayout = named "wide" $ avoidStruts $ Mirror basicLayout
singleLayout = named "single" $ avoidStruts $ noBorders Full
fullscreenLayout = named "fullscreen" $ noBorders Full
imLayout = avoidStruts $ reflectHoriz $ withIMs ratio rosters chatLayout where
    chatLayout      = GridRatio 1.0
    ratio           = 1%6
    rosters         = [skypeRoster, pidginRoster]
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
    skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

myLayoutHook = smartBorders (fullscreen $ im $ normal) where
    normal     = mkToggle (single NBFULL) (tallLayout ||| wideLayout)
    fullscreen = onWorkspace "9" fullscreenLayout
    im         = onWorkspace "10" imLayout

-- special treatment for specific windows:
-- put the Pidgin and Skype windows in the im workspace
myManageHook = manageDocks <+> fullscreenManageHooks <+> imManageHooks <+> floatManageHooks <+> manageHook myBaseConfig
-- fullscreenManageHooks = composeAll [isFullscreen --> (doF S.focusDown <+> doFullFloat)]
fullscreenManageHooks = composeOne [transience, isFullscreen -?> doFullFloat]
imManageHooks = composeAll [isIM --> moveToIM] where
    isIM     = foldr1 (<||>) [isPidgin, isSkype]
    isPidgin = className =? "Pidgin"
    isSkype  = className =? "Skype"
    moveToIM = doF $ S.shift "10"

floatManageHooks = composeAll [shouldFloat --> doFloat] where
    shouldFloat = stringProperty "WM_NAME" =? "Crack Attack!"

-- Mod4 is the Super / Windows key
myModMask = mod4Mask
altMask = mod1Mask

myKeys conf = M.fromList $
    [ ((myModMask              , xK_Return), spawn $ XMonad.terminal conf)
    , ((myModMask              , xK_space), windows $ withOtherWorkspace S.view)
    , ((myModMask .|. controlMask, xK_space), windows $ withOtherWorkspace S.greedyView)
    , ((myModMask .|. shiftMask, xK_e     ), spawn "e -c")
    , ((altMask .|. controlMask, xK_l     ), spawn "sxlock -f fixed")
    , ((myModMask,               xK_p     ), spawn "dmenu-launch") -- %! Launch dmenu
    , ((myModMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((myModMask              , xK_i     ), spawn "xcalib -a -i")
    , ((myModMask              , xK_c     ), kill)
    , ((myModMask .|. shiftMask, xK_space ), sendMessage NextLayout)
    , ((myModMask              , xK_n     ), refresh)
    , ((myModMask              , xK_m     ), windows S.swapMaster)
    , ((altMask                , xK_Tab   ), windows S.focusDown)
    , ((altMask .|. shiftMask  , xK_Tab   ), windows S.focusUp)
    , ((myModMask              , xK_Down  ), windows S.swapDown)
    , ((myModMask              , xK_Up    ), windows S.swapUp)
    , ((myModMask              , xK_Left  ), sendMessage Shrink)
    , ((myModMask              , xK_Right ), sendMessage Expand)
    , ((myModMask              , xK_a     ), withFocused (keysMoveWindowTo (512,384) (1%2,1%2)))
    , ((myModMask              , xK_t     ), withFocused $ windows . S.sink)
    , ((myModMask              , xK_F11   ), sendMessage $ Toggle NBFULL)
    , ((myModMask              , xK_b     ), sendMessage (IncMasterN 1))
    , ((myModMask              , xK_v     ), sendMessage (IncMasterN (-1)))
    , ((myModMask .|. controlMask, xK_q   ), broadcastMessage ReleaseResources >> restart "xmonad" True)
    , ((myModMask .|. controlMask, xK_backslash   ), broadcastMessage ReleaseResources >> restart "stumpwm" True)
    , ((myModMask .|. shiftMask .|. controlMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((myModMask .|. shiftMask  , xK_Left  ), prevWS)
    , ((myModMask .|. shiftMask  , xK_Right ), nextWS)
    ] ++
    -- Alt/Win+F1..F10 switches to workspace
    -- (Alt is in a nicer location for the thumb than the Windows key,
    -- and 1..9 keys are already in use by Firefox, irssi, ...)
    [((m .|. mod, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F10]
    , (f, m) <- [(S.greedyView, 0),
                 (liftM2 (.) S.greedyView S.shift, controlMask)]
    , mod <- [myModMask, altMask]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(S.view, 0), (liftM2 (.) S.view S.shift, controlMask)]]


withOtherWorkspace f ws = f (otherWorkspace ws) ws
  where
    otherWorkspace = S.tag . S.workspace . head . S.visible

-- move mouse cursor when we switch focus by keyboard
myLogHook = updatePointer (Relative 0.5 0.5)

-- xmobar
myPP = xmobarPP
               { ppTitle = xmobarColor "#d33682" ""
               , ppCurrent = xmobarColor "#4040ff" ""
               , ppLayout = const ""
               }
toggleMobarKey XConfig {XMonad.modMask = modMask} = (modMask, xK_F12)

-- mouse bindings that mimic Gnome's
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((myModMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((myModMask, button3), (\w -> focus w >> mouseResizeWindow w))
    , ((myModMask .|. shiftMask, button1), (\w -> focus w >> (withFocused $ windows . S.sink)))
    , ((myModMask, button4), (const $ windows S.swapUp))
    , ((myModMask, button5), (const $ windows S.swapDown))
    ]

-- put it all together
main = xmonad =<< statusBar "xmobar" myPP toggleMobarKey myBaseConfig
    { modMask = myModMask
    , workspaces = myWorkspaces
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , handleEventHook = fullscreenEventHook
    , logHook = myLogHook
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , terminal = myTerminal
    , keys = myKeys
    , mouseBindings = myMouseBindings
    }

-- modified version of XMonad.Layout.IM --

-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)

instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"

-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props

-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid

hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w

-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> S.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = S.stack wksp
    let ws = S.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= S.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {S.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)
