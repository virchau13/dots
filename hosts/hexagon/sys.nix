{ inputs, config, lib, pkgs, ... }:

{
    imports = [
        ./hw.nix
        ../../apps/hyprland/sys.nix
        ../../apps/backup
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
            steam = super.steam.override {
                extraPkgs = p: with p; [ libkrb5 ];
            };
            python3 = super.python3.override {
                packageOverrides = py-self: py-super: {
                    fontmake = py-super.fontmake.overridePythonAttrs(old: {
                        # TODO remove
                        doCheck = false;
                    });
                };
            };
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

    # NOTE: change when rtl8192eu gets fixed for later kernel versions
    boot = let kernelPackages = pkgs.linuxKernel.packages.linux_6_6; in {
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
            # obs camera
            v4l2loopback
            # wifi
            rtl8192eu
        ];
        kernelModules = [ "coretemp" "it87" "lm92" "k10temp" "amdgpu" "rtl8192eu" ];
        kernelParams = [ "delayacct" "boot.shell_on_fail" ];
        # enable sysrq (https://github.com/NixOS/nixpkgs/issues/83694)
        kernel.sysctl."kernel.sysrq" = 1;
    };

    networking.hostName = "hexagon";

    # Set your time zone.
    time.timeZone = "America/Toronto";

    # use systemd-networkd
    networking.useDHCP = true;
    # systemd.network = {
    #     enable = true;
    #     networks = {
    #         "20-enp10s0" = {
    #             matchConfig.Name = "enp10s0";
    #             networkConfig = {
    #                 DHCP = "ipv6";
    #                 Address = "192.168.0.211/24";
    #                 Gateway = "192.168.0.1";
    #                 DNS = "1.1.1.1";
    #                 IPv6AcceptRA = "yes";
    #                 LinkLocalAddressing = "yes";
    #                 MulticastDNS = "yes"; # resolves .local addresses
    #                 # for docker
    #                 IPMasquerade = "yes";
    #                 IPForward = "yes";
    #             };
    #         };
    #     };
    #     links = {
    #         "30-docker-unmanaged" = {
    #             matchConfig.OriginalName = "docker0";
    #             extraConfig = ''
    #                 [Link]
    #                 Unmanaged=yes
    #             '';
    #         };
    #     };
    # };

    networking.wireguard.enable = true;
    networking.wg-quick = {
        interfaces = {
            wg0 = {
                address = [ "10.200.200.13/24" ];
                listenPort = 5298;
                privateKeyFile = "/run/secrets/wg/privkey";
                peers = [
                    {
                        publicKey = "ODEdIe46o4+tGe1biG2vCn+3wUk3pO5iFdvXDIGbGzo=";
                        allowedIPs = [ "10.200.200.0/24" ];
                        endpoint = "almizan.aquila.n.hexular.net:5298";
                        persistentKeepalive = 25;
                    }
                ];
            };
        };
    };

    # networking.wireless = {
    #     enable = true;
    #     environmentFile = "/run/secrets/wifi/env";
    #     networks = {
    #         "SHLNA 2.4" = {
    #             psk = "@PASSWD@";
    #             # for some reason this is the only one that works reliably
    #             # extraConfig = ''
    #             #     freq_list=2462
    #             # '';
    #         };
    #     };
    # };

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
        font = "Lat2-Terminus16";
        keyMap = "us";
    };

    services.xserver = {
        enable = true;
        displayManager.startx.enable = true;
        videoDrivers = [ "amdgpu" ];
    };

    services.libinput = {
        enable = true;
        mouse = {
            accelProfile = "flat";
            middleEmulation = false;
        };
    };

    services.printing.enable = true;

    services.netdata = {
        enable = true;
        config = {
            "health" = {
                "enabled alarms" = 
                    "!30min_ram_swapped_out !used_swap !inbound_packets_dropped_ratio"
                    + " !system_clock_sync_state !1m_received_traffic_overflow"
                    + " *";
            };
        };
    };

    services.udev.packages = with pkgs; [
        (pkgs.qt5.callPackage ../../apps/xp-pen/unwrapped.nix {})
    ];

    security.rtkit.enable = true;
    services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
    };

    hardware.graphics = {
        enable = true;
    };

    # For Corsair keyboard control.
    hardware.ckb-next.enable = true;

    users.users.hexular = {
        isNormalUser = true;
        extraGroups = [
            "wheel" # sudo
            "networkmanager"
            "docker"
            "libvirtd"
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
        bpftrace
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
        adwaita-icon-theme
        xclip
        kdePackages.breeze-icons
        inkscape
        cargo-flamegraph
        gthumb
        krita
        rr
        wireshark
        iw
        xdotool
        lm_sensors
        (pkgs.callPackage ../../apps/xp-pen {})
        yubikey-personalization
        ungoogled-chromium
        inputs.nix-gaming.packages.${pkgs.system}.wine-ge
        # i know right?
        omnisharp-roslyn
        pipx
        turbovnc
        docker-credential-helpers
        terraform
        terraform-ls
        qemu
        virt-manager
        framesh
        gamescope # for steam
        gamemode

        ckan

        # wayland
        xwayland
        arc-theme
        linuxPackages.v4l2loopback
        v4l-utils
        
        # wxmaxima
        # maxima

        r2modman

        emacs

        android-tools

        docker-compose
        ghidra

        spotify
    ];

    programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
        pinentryPackage = pkgs.pinentry.tty;
    };

    programs.dconf.enable = true;

    # Enable the OpenSSH daemon.
    services.openssh = {
        enable = true;
        ports = [ 22 22000 ];
    };

    programs.mosh.enable = true;

    programs.sway.enable = true;

    systemd.services = { 
        ttyd = {
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
        aquila-dns-updater = {
            requires = [ "network-online.target" ];
            serviceConfig = {
                User = "hexular";
                WorkingDirectory = "/home/hexular";
            };
            # update my IP on altair
            script = ''
                ${pkgs.dig}/bin/dig @resolver4.opendns.com myip.opendns.com +short | ${pkgs.openssh}/bin/ssh altair 'cat > /var/lib/dnscontrol/chechia-ip'
            '';
        };
    };

    systemd.timers.aquila-dns-updater = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "2m"; # 2 minutes after boot
            OnUnitActiveSec = "10m"; # every 10 minutes
            Unit = "aquila-dns-updater.service";
        };
    };

    # Open ports in the firewall.
    # networking.firewall.allowedTCPPorts = [ ... ];
    # networking.firewall.allowedUDPPorts = [ ... ];
    # Or disable the firewall altogether.(TODO change this to true)
    networking.firewall.enable = false;

    programs.steam.enable = true;
    
    # replace systemd-timesyncd
    services.chrony.enable = true;

    services.dbus.enable = true;
    xdg.portal = {
        enable = true;
        wlr.enable = true;
        extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
        config.common.default = "*"; # TODO change this
    };
    xdg.autostart.enable = true;

    # virtualisation.virtualbox.host.enable = true;
    # virtualisation.virtualbox.host.enableExtensionPack = true;
    # users.extraGroups.vboxusers.members = [ "hexular" ];
    virtualisation.podman = {
        enable = true;
    };

    virtualisation.libvirtd.enable = true;

    virtualisation.docker = {
        enable = true;
    };

    # virtualisation.oci-containers = {
    #     backend = "docker";
    #     containers.neko = {
    #         image = "m1k1o/neko:xfce";
    #     };
    # };

    # i18n.inputMethod = {
    #     enable = true;
    #     type = "fcitx5";
    #     fcitx5 = {
    #         waylandFrontend = true;
    #         addons = with pkgs; [ fcitx5-chinese-addons ];
    #     };
    # };

    # for m1k1o/neko
    services.nginx = {
        enable = true;
        config = builtins.readFile ./nginx.conf;
    };

    programs.kdeconnect.enable = true;

    programs.sysdig.enable = true;

    programs.nbd.enable = true;

    hardware.i2c.enable = true;

    # HACK: get hard-coded HIP GPU acceleration to work
    systemd.tmpfiles.rules = [
        "L+	/opt/rocm/hip	-	-	-	-	${pkgs.rocmPackages.clr}"
    ];

    services.custom-backup = {
        enable = true;
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
