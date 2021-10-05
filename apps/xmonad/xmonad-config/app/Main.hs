module Main where

import Lib
import System.Environment
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

startup :: X ()
startup = do
  spawnOnce
    "picom \
    \ --xrender-sync-fence --experimental-backends --backend glx \
    \ --use-damage --vsync --config /dev/null --show-all-xerrors \
    \ --glx-no-stencil &> ~/.cache/picom.out"
  spawnOnce "dunst &> ~/.cache/dunst.out"

main :: IO ()
main =
  let modMask = mod4Mask
   in xmonad $
        ewmh
          desktopConfig
            { modMask = modMask, -- Super key
              startupHook = startup,
              terminal = "alacritty"
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
                             ((modMask .|. shiftMask, xK_s), spawn "flameshot gui")
                           ]
