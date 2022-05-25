import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

startup :: X ()
startup = do
    spawnOnce "picom \
        \ --xrender-sync-fence --experimental-backends --backend glx \
        \ --use-damage --vsync --config /dev/null --show-all-xerrors \
        \ --glx-no-stencil --log-level INFO --log-file ~/.cache/picom.out"


main :: IO ()
main = let
        modMask = mod4Mask
    in (xmonad . ewmhFullscreen . ewmh . docks) $ def {
            modMask = modMask,
            startupHook = startup,
            terminal = "alacritty",
            layoutHook = avoidStruts $ layoutHook def
        } `removeKeysP` [
            "M-S-/" -- xmessage help
        ] `additionalKeysP` [
            ("M-q", spawn "M=~/.xmonad/xmonad-x86_64-linux; \
                \ notify-desktop 'Recompiling' && $M --recompile && notify-desktop 'Restarting' && \
                \ $M --restart && notify-desktop 'Done!'"),
            ("M-S-s", spawn "flameshot gui"),
            ("M-S-s", spawn "notify-desktop \"$(xcolor)\""),
            ("M-<Space>", spawn "rofi -show run")
        ]
