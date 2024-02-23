{ ... }:
{
    # Emacs uses ~/.Xresources instead of fontconfig.
    # Yes, even on Wayland. Sigh.
    xresources.properties = {
        "Xft.rgba" = "rgb";
        "Xft.lcdfilter" = "lcddefault";
        "Xft.hinting" = "1";
        "Xft.hintstyle" = "hintfull";
    };
}
