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
        # why this is not enabled by default boggles me
        # makes sure /tmp is mounted on tmpfs
        tmpOnTmpfs = true;
        kernelParams = [ "delayacct" ];
    };

    hardware.nvidia = {
        modesetting.enable = true;
    };

    networking.hostName = "hexagon";

    # Set your time zone.
    time.timeZone = "Asia/Singapore";

    # set global useDHCP to false
    networking.useDHCP = false;
    networking.interfaces = {
        enp5s0 = {
            useDHCP = true;
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
    # hardware.ckb-next.enable = true;

    users.users.hexular = {
        isNormalUser = true;
        extraGroups = [ 
            "wheel" # sudo
            "networkmanager"
            # "docker"
            # "lxd"
        ];
        shell = pkgs.zsh;
    };

    documentation.dev.enable = true;

    environment.systemPackages = with pkgs; let 
        pythonPackages = python-pkgs: with python-pkgs; [
            pandas
            xkcdpass
            ipython
            jupyter
            flask
            pyjwt
        ];
        python = python3.withPackages pythonPackages;
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
        alsaUtils
        htop
        cmake
        gnumake
        killall
        feh
        xcolor
        python
        gnome3.adwaita-icon-theme
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
        clang-tools
        lm_sensors
        (libsForQt5.callPackage ../../apps/xp-pen-deco-01-v2-driver {})
        yubikey-personalization
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
        ports = [ 22 25565 ];
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

    virtualisation.virtualbox.host.enable = true;
    virtualisation.virtualbox.host.enableExtensionPack = true;
    users.extraGroups.vboxusers.members = [ "hexular" ];

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "21.05"; # Did you read the comment?

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

