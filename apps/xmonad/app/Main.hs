module Main where

import Lib
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import System.Environment
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W

startup :: X ()
startup = do
  spawnOnce
    "killall picom; echo > ~/.cache/picom.out; picom \
    \ --xrender-sync-fence --experimental-backends --backend glx \
    \ --use-damage --vsync --config /dev/null --show-all-xerrors \
    \ --glx-no-stencil --log-level INFO --log-file ~/.cache/picom.out"
  -- spawnOnce "killall dunst; dunst &> ~/.cache/dunst.out"

-- Let docks stay docks
-- layout = avoidStruts (tiled ||| Mirror tiled ||| Full)
--     where
--         -- default tiling algorithm partitions the screen into two panes
--         tiled   = Tall nmaster delta ratio
-- 
--         -- The default number of windows in the master pane
--         nmaster = 1
-- 
--         -- Default proportion of screen occupied by master pane
--         ratio   = 1/2
-- 
--         -- Percent of screen to increment by when resizing panes
--         delta   = 3/100

-- eventLogHook :: X ()
-- eventLogHook = do
--     winset <- gets windowset
--     title <- maybe (return "") (fmap show . getName) . W.peek $ winset
--     let currWs = W.currentTag winset
--     let wss = map W.tag $ W.workspaces winset
--     let wsStr = join $ map (fmt currWs) $ sort' wss
-- 
--     io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
--     io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")
-- 
--     where fmt currWs ws
--             | currWs == ws = "[" ++ ws ++ "]"
--             | otherwise    = " " ++ ws ++ " "
--           sort' = sortBy (compare `on` (!! 0))

main :: IO ()
main = do 
    -- Set up pipes to let Polybar know of workspaces
    -- forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
    -- Main config
    let modMask = mod4Mask in xmonad $
        (ewmhFullscreen . ewmh . docks)
           def
             { modMask = modMask, -- Super key
               startupHook = startup,
               terminal = "alacritty",
               layoutHook = avoidStruts $ layoutHook def,
               handleEventHook = handleEventHook def
               -- logHook = eventLogHook
             }
           `removeKeys` [ (modMask .|. shiftMask, xK_slash) -- xmessage help
                        ]
           `additionalKeys` [ ( (modMask, xK_q),
                                spawn
                                  "notify-desktop 'Recompiling...' && \
                                  \ ~/.xmonad/xmonad-x86_64-linux --recompile && \
                                  \ ~/.xmonad/xmonad-x86_64-linux --restart && \
                                  \ notify-desktop 'Done!'"
                              ),
                              ((modMask, xK_p), spawn "rofi -show run"),
                              ((modMask .|. shiftMask, xK_s), spawn "flameshot gui"),
                              ((modMask, xK_c), spawn "notify-desktop \"$(xcolor)\""),
                              ((modMask .|. controlMask, xK_t), spawn "notify-desktop \"$(date)\"")
                            ]
