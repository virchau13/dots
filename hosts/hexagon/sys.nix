{ inputs, config, lib, pkgs, ... }:

{
    imports = [ 
        ./hw.nix 
    ];

    nix = {
        settings = {
            auto-optimise-store = true;
            # i don't want all my cores to be taken up
            cores = 4; # cores per derivation built
            substituters = [
                "https://nix-gaming.cachix.org"
                "https://cuda-maintainers.cachix.org"
            ];
            trusted-public-keys = [ 
                "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
                "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
            ];
        };
        # increase system responsiveness during nix-build
        # https://github.com/NixOS/nixpkgs/pull/138741#issuecomment-979918607
        daemonCPUSchedPolicy = "idle";
        daemonIOSchedClass = "idle";
    };

    nixpkgs.overlays = [
        (self: super: {
            steam-fhsenv = super.steam-fhsenv.override (old: {
                extraPkgs = with pkgs.pkgsi686Linux; [ gperftools ];
            });
            omnisharp-rosyln = (super.omnisharp-roslyn.override(old: {
                dotnetCorePackages = with self.dotnetCorePackages; {
                    sdk_6_0 = sdk_7_0;
                    runtime_6_0 = runtime_7_0;
                };
            }));
            msbuild = (super.msbuild.override(old: {
                dotnetCorePackages = with self.dotnetCorePackages; {
                    sdk_6_0 = sdk_7_0;
                    runtime_6_0 = runtime_7_0;
                };
            }));
        })
    ];

    sops = {
        defaultSopsFile = ./secrets.yaml;
        age.keyFile = "/home/hexular/.config/sops/age/keys.txt";
        secrets = {
            "wg/privkey" = {};
            "wifi/env" = {};
            "scripts/refresh-playlist" = {
                mode = "0500";
                owner = config.users.users.hexular.name;
            };
            "ttyd/ca.pem" = {
                owner = "hexular";
            };
            "ttyd/ca-key.pem" = {
                owner = "hexular";
            };
            "ttyd/http-auth" = {
                owner = "hexular";
            };
            "guac/ca.key" = { owner = "nginx"; };
            "guac/ca.cert" = { owner = "nginx"; };
            "guac/client.cert" = { owner = "nginx"; };
        };
    };

    # Without this my CPU defaults to 1.4GHz frequency when it should be running at 3.7GHz.
    powerManagement.cpuFreqGovernor = "performance";

    boot = let kernelPackages = pkgs.linuxKernel.packages.linux_xanmod; in {
        inherit kernelPackages;
        # Use the systemd-boot EFI boot loader.
        loader.systemd-boot = {
            enable = true;
            # Use high resolution in TTY.
            consoleMode = "max";
        };
        loader.efi.canTouchEfiVariables = true;
        extraModulePackages = with kernelPackages; [ 
            # For lm_sensors
            it87
        ];
        kernelModules = [ "coretemp" "it87" "lm92" "k10temp" ];
        kernelParams = [ "delayacct" "boot.shell_on_fail" ];
    };

    hardware.nvidia = {
        modesetting.enable = true;
    };

    networking.hostName = "hexagon";

    # Set your time zone.
    time.timeZone = "Asia/Singapore";

    # use systemd-networkd
    networking.useDHCP = false;
    systemd.network = {
        enable = true;
        networks = {
            "20-enp5s0" = {
                matchConfig.Name = "enp5s0";
                networkConfig = {
                    DHCP = "ipv6";
                    Address = "192.168.0.211/24";
                    Gateway = "192.168.0.1";
                    DNS = "1.1.1.1";
                    IPv6AcceptRA = "yes";
                    LinkLocalAddressing = "yes";
                    MulticastDNS = "yes"; # resolves .local addresses
                    # for docker
                    IPMasquerade = "yes";
                    IPForward = "yes";
                };
            };
        };
        links = {
            "30-docker-unmanaged" = {
                matchConfig.OriginalName = "docker0";
                extraConfig = ''
                    [Link]
                    Unmanaged=yes
                '';
            };
        };
    };

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
        font = "Lat2-Terminus16";
        keyMap = "us";
    };

    services.xserver = {
        enable = true;
        displayManager.startx.enable = true;
        libinput = {
            enable = true;
            mouse = {
                accelProfile = "flat";
                middleEmulation = false;
            };
        };
        videoDrivers = [ "nvidia" ];
    };

    services.printing.enable = true;

    services.netdata = {
        enable = true;
    };

    services.udev.packages = with pkgs; [ yubikey-personalization ];

    # Enable sound.
    sound.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
    };

    hardware.opengl = {
        enable = true;
        driSupport = true;
        driSupport32Bit = true;
    };

    # For Corsair keyboard control.
    hardware.ckb-next.enable = true;

    users.users.hexular = {
        isNormalUser = true;
        extraGroups = [ 
            "wheel" # sudo
            "networkmanager"
            "docker"
            # "lxd"
        ];
        shell = pkgs.zsh;
    };

    documentation.dev.enable = true;

    environment.systemPackages = with pkgs; let 
    in [
        vim
        wget
        firefox
        borgbackup
        kitty.terminfo # for hexamac to have proper terminfo support
        git
        gcc
        bcc # messing with ebpf
        moreutils # errno, etc
        man-pages
        man-pages-posix
        gdb
        zsh
        p7zip
        glxinfo
        wl-clipboard
        alsa-utils
        htop
        cmake
        gnumake
        killall
        feh
        xcolor
        gnome.adwaita-icon-theme
        xclip
        breeze-icons
        inkscape
        cargo-flamegraph
        gthumb
        krita
        rr
        wireshark
        iw
        xdotool
        # switch to 15 because of https://github.com/clangd/clangd/issues/1188
        clang-tools_15
        lm_sensors
        (xp-pen-deco-01-v2-driver.overrideAttrs(old: {
          src = fetchzip {
              url = "https://www.xp-pen.com/download/file/id/1936/pid/440/ext/gz.html#.tar.gz";
              name = "xp-pen-deco-01-v2-driver-3.2.3.220323-1.tar.gz";
              sha256 = "sha256-CV4ZaGCFFcfy2J0O8leYgcyzFVwJQFQJsShOv9B7jfI=";
          };
        }))
        yubikey-personalization
        ungoogled-chromium
        inputs.nix-gaming.packages.${pkgs.system}.wine-ge
        # i know right?
        omnisharp-roslyn
        dotnet-sdk_7
        msbuild
        pipx
        turbovnc
        docker-credential-helpers
    ];

    # get va-api working in firefox
    environment.sessionVariables = {
        LIBVA_DRIVER_NAME = "nvidia";
        MOZ_DISABLE_RDD_SANDBOX = "1";
    };

    programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
        pinentryFlavor = "gtk2";
    };

    programs.dconf.enable = true;

    # Enable the OpenSSH daemon.
    services.openssh = {
        enable = true;
        ports = [ 22 25565 22000 ];
    };

    programs.mosh.enable = true;

    systemd.services.ttyd = {
        serviceConfig = {
            User = "hexular";
            WorkingDirectory = "/home/hexular";
        };
        requires = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
        script = ''
            ${pkgs.ttyd}/bin/ttyd \
                --port 1051 \
                --credential "$(cat /run/secrets/ttyd/http-auth)" \
                --base-path /tty \
                --index /home/hexular/prog/repos/ttyd/html/dist/inline.html \
                --ipv6 \
                --ssl \
                --ssl-cert /run/secrets/ttyd/ca.pem \
                --ssl-key /run/secrets/ttyd/ca-key.pem \
                --ssl-ca /run/secrets/ttyd/ca.pem \
                ${pkgs.zsh}/bin/zsh
        '';
    };

    # Open ports in the firewall.
    # networking.firewall.allowedTCPPorts = [ ... ];
    # networking.firewall.allowedUDPPorts = [ ... ];
    # Or disable the firewall altogether.(TODO change this to true)
    networking.firewall.enable = false;

    programs.steam.enable = true;

    xdg.portal = {
        enable = true;
        extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    };
    xdg.autostart.enable = true;

    # virtualisation.virtualbox.host.enable = true;
    # virtualisation.virtualbox.host.enableExtensionPack = true;
    # users.extraGroups.vboxusers.members = [ "hexular" ];
    virtualisation.podman = {
        enable = true;
    };

    virtualisation.docker = {
        enable = true;
        enableNvidia = true;
    };

    # virtualisation.oci-containers = {
    #     backend = "docker";
    #     containers.neko = {
    #         image = "m1k1o/neko:xfce";
    #     };
    # };

    i18n.inputMethod = {
        enabled = "fcitx";
        fcitx.engines = with pkgs.fcitx-engines; [ libpinyin ];
    };

    # for m1k1o/neko
    services.nginx = {
        enable = true;
        config = builtins.readFile ./nginx.conf;
    };

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "22.05"; # Did you read the comment?

    home-manager.extraSpecialArgs = let 
        homeDir = config.users.users.hexular.home; 
    in { 
        inherit inputs homeDir;
        configDir = "${homeDir}/.config/nixpkgs";
    };
    home-manager.users.hexular = { pkgs, ... }: {
        imports = [ 
            ./home.nix 
        ];
    };
}

