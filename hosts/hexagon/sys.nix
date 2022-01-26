{ inputs, config, lib, pkgs, ... }:

{
    imports = [ 
        ./hw.nix 
    ];

    nix.autoOptimiseStore = true;

    sops = {
        defaultSopsFile = ./secrets.yaml;
        age.keyFile = "/home/hexular/.config/sops/age/keys.txt";
        secrets = {
            "wg/privkey" = {};
            "scripts/refresh-playlist" = {
                mode = "0500";
                owner = config.users.users.hexular.name;
            };
        };
    };

    # Without this my CPU defaults to 1.4GHz frequency when it should be running at 3.7GHz.
    powerManagement.cpuFreqGovernor = "performance";

    boot = let kernel = "linux_xanmod"; in {
        kernelPackages = pkgs.linuxKernel.packages."${kernel}";
        # Use the systemd-boot EFI boot loader.
        loader.systemd-boot = {
            enable = true;
            # Use high resolution in TTY.
            consoleMode = "max";
        };
        loader.efi.canTouchEfiVariables = true;
        # To get lm_sensors to work
        extraModulePackages = with pkgs.linuxKernel.packages."${kernel}"; [ it87 rtl8192eu ];
        kernelModules = [ "coretemp" "it87" "lm92" "k10temp" "rtl8192eu" ];
        # Enable WireGuard logs
        kernelParams = [ "wireguard.dyndbg=\"module wireguard +p\"" ];
    };

    networking.hostName = "hexagon";

    # Set your time zone.
    time.timeZone = "Asia/Singapore";

    networking.interfaces = {
        enp5s0 = {
            useDHCP = true;
        };
        # wlp2s0f0u4 = {
        #     useDHCP = true;
        # };
    };

    networking.wireguard.enable = true;
    networking.wg-quick = {
        interfaces = {
            wg0 = {
                address = [ "10.200.200.13/24" ];
                dns = [ "1.1.1.1" ];
                listenPort = 5298;
                privateKeyFile = "/run/secrets/wg/privkey";
                peers = [
                    {
                        publicKey = "ODEdIe46o4+tGe1biG2vCn+3wUk3pO5iFdvXDIGbGzo=";
                        # allowedIPs = [ "0.0.0.0/0" ];
                        allowedIPs = [ "10.200.200.0/24" ];
                        endpoint = "io.hexular.net:5298";
                        persistentKeepalive = 25;
                    }
                ];
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

    # Enable sound.
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
        # for VA-API support on Nvidia
        extraPackages = [ pkgs.vaapiVdpau ];
    };

    # https://forums.developer.nvidia.com/t/bug-nvidia-v495-29-05-driver-spamming-dbus-enabled-applications-with-invalid-messages/192892/11
    systemd.services.nvidia-fake-powerd = {
        description = "NVIDIA fake powerd service";
        wantedBy = [ "default.target" ];
        aliases = ["dbus-nvidia.fake-powerd.service"];
        serviceConfig = {
            Type = "dbus";
            User = "messagebus";
            Group = "messagebus";
            BusName = "nvidia.powerd.server";
            LimitNPROC = 2;
            ExecStart = "${pkgs.dbus}/bin/dbus-test-tool black-hole --system --name=nvidia.powerd.server";
            ProtectHome = true;
            ProtectSystem = "full";
        };
    };
    services.dbus.packages = [
        (pkgs.writeTextFile {
            name = "nvidia-fake-powerd.conf";
            destination = "/share/dbus-1/system.d/nvidia-fake-powerd.conf";
            text = ''
                <!DOCTYPE busconfig PUBLIC
                 "-//freedesktop//DTD D-BUS Bus Configuration 1.0//EN"
                 "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
                <busconfig>
                        <policy user="messagebus">
                                <allow own="nvidia.powerd.server"/>
                        </policy>
                        <policy context="default">
                                <allow send_destination="nvidia.powerd.server"/>
                                <allow receive_sender="nvidia.powerd.server"/>
                        </policy>
                </busconfig>
            '';
        })
    ];

    # For Corsair keyboard control.
    hardware.ckb-next.enable = true;

    users.users.hexular = {
        isNormalUser = true;
        extraGroups = [ 
            "wheel" # sudo
            "docker"
            "lxd"
        ];
        shell = pkgs.zsh;
    };

    environment.systemPackages = with pkgs; let 
        pythonPackages = python-pkgs: with python-pkgs; [
            pandas
            xkcdpass
            ipython
            jupyter
        ];
        python = python3.withPackages pythonPackages;
    in [
        vim
        wget
        firefox
        borgbackup
        kitty # for hexamac to have proper terminfo support
        git
        gcc
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
        nix-index
        gnome3.adwaita-icon-theme
        xclip
        breeze-icons
        inkscape
        cargo-flamegraph
        gthumb
    ];

    programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
        pinentryFlavor = "curses";
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

    xdg.portal.enable = true;
    xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

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

