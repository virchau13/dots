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
        delta = {
            enable = true;
            options = {
                line-numbers = true;
                side-by-side = true;
                wrap-max-lines = "unlimited";
            };
        };
        userName = "virchau13";
        userEmail = "virchau13@hexular.net";
        signing = {
            signByDefault = false; # burned too many times
            key = "AA1BA03FFF02700DFD836BD325B242ED74B61B15";
        };
        extraConfig.http.cookiefile = "/home/hexular/.gitcookies";
    };

    home.packages = with pkgs; [
      pavucontrol
      pass
      pamixer
      notify-desktop
      easyeffects
      mpv
      wofi
      vesktop
      pinentry.tty
      (pkgs.flameshot.overrideAttrs(old: {
          version = "12.2.0-alpha";
          src = pkgs.fetchFromGitHub {
              owner = "flameshot-org";
              repo = "flameshot";
              rev = "3d21e4967b68e9ce80fb2238857aa1bf12c7b905";
              sha256 = "sha256-OLRtF/yjHDN+sIbgilBZ6sBZ3FO6K533kFC1L2peugc=";
          };
          buildInputs = old.buildInputs ++ [ pkgs.libsForQt5.kguiaddons ];
          patches = [ ./flameshot-fix-clipboard.patch ];
          cmakeFlags = [ "-DUSE_WAYLAND_GRIM=1" "-DUSE_WAYLAND_CLIPBOARD=true" ];

          postInstall = ''
              wrapProgram $out/bin/flameshot \
                  --prefix PATH : ${pkgs.grim}/bin
          '';
      }))
    ];

    programs.alacritty = {
        enable = true;
        settings = {
            font.normal.family = "Hex Mono";
            font.size = 10.5;
        };
    };

    services.gpg-agent = {
        enable = true;
        pinentryPackage = pkgs.pinentry.tty;
    };

    programs.tmux = {
        enable = true;
        clock24 = true;
        mouse = true;
        extraConfig = ''
            set-option -sa terminal-overrides ",xterm*:Tc"
        '';
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
