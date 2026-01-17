{ inputs, configDir, config, pkgs, lib, ... }: 
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
in {
    imports = [
        ../../apps/nvim
    ];

    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;

    home.file = {
        ".zshrc".source = mkOutOfStoreSymlink "${configDir}/apps/zsh/zshrc";
        ".zshenv".source = mkOutOfStoreSymlink "${configDir}/apps/zsh/zshenv";
        "bin".source = mkOutOfStoreSymlink "${configDir}/bin";
        ".cargo/config.toml".text = ''
            [build]
            rustc-wrapper = "${pkgs.sccache}/bin/sccache"

            [registries.crates-io]
            protocol = "sparse"
        '';
    };

    xdg.configFile = {
        "wezterm".source = mkOutOfStoreSymlink "${configDir}/apps/wezterm";
        "sway".source = mkOutOfStoreSymlink "${configDir}/apps/sway";
    };

    # Extra $PATH directories
    home.sessionPath = [ "${config.home.homeDirectory}/bin" ];

    programs.git = {
        enable = true;
        settings = {
            user.email = "virchau13@hexular.net";
            user.name = "Vir Chaudhury";
            http.cookiefile = "/home/hexular/.gitcookies";
        };
        signing = {
            signByDefault = false; # burned too many times
            key = "AA1BA03FFF02700DFD836BD325B242ED74B61B15";
        };
    };

    programs.delta = {
        enable = true;
        enableGitIntegration = true;
        options = {
            line-numbers = true;
            side-by-side = true;
            wrap-max-lines = "unlimited";
        };
    };

    home.packages = with pkgs; [
      pavucontrol
      pass
      pamixer
      notify-desktop
      easyeffects
      mpv
      wofi
      pinentry-curses
    ];

    programs.alacritty = {
        enable = true;
        settings = {
            font.normal.family = "Hex Mono";
            font.size = 10.5;
            # font.size = 9;
        };
    };

    services.gpg-agent = {
        enable = true;
        pinentry.package = pkgs.pinentry-curses;
    };

    programs.tmux = {
        enable = true;
        clock24 = true;
        mouse = true;
        extraConfig = ''
            set-option -sa terminal-overrides ",xterm*:Tc"
        '';
    };

    dconf.settings = {
        "org/gnome/desktop/interface" = {
            font-antialiasing = "rgba";
        };
    };

    xresources.properties = {
        "Xft.rgba" = "rgb";
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "21.05";
}
