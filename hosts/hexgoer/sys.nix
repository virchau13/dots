{ inputs, config, lib, pkgs, ... }:

{
  imports =
    [
      ./hw.nix
    ];

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
      };
      efi.canTouchEfiVariables = true;
    };
    initrd = {
      availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
      kernelModules = [ ];
    };
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
    kernelParams = [ "boot.shell_on_fail" ];
    # enable SysRq key
    kernel.sysctl."kernel.sysrq" = 1;
  };

  networking.hostName = "hexgoer";
  networking.networkmanager = {
    enable = true;
    # dhcp = "dhcpcd";
    plugins = with pkgs; [
      networkmanager-openvpn
    ];
  };
  # networking.dhcpcd.enable = true;
  # https://github.com/NixOS/nixpkgs/issues/341092
  # networking.useDHCP = lib.mkForce true;
  # systemd.services.dhcpcd.enable = false; # This is the new line
  #environment.etc."dhcpcd.conf".source = let
  #    q = config.systemd.services.dhcpcd.serviceConfig.ExecStart;
  #    l = builtins.split " " q;
  #  in builtins.elemAt l (builtins.length l - 1);
  #environment.etc."dhcpcd.conf".text = ''
  #  # Inform the DHCP server of our hostname for DDNS.
  #  hostname

  #  # A list of options to request from the DHCP server.
  #  option domain_name_servers, domain_name, domain_search
  #  option classless_static_routes, ntp_servers, interface_mtu

  #  # A ServerID is required by RFC2131.
  #  # Commented out because of many non-compliant DHCP servers in the wild :(
  #  #require dhcp_server_identifier

  #  # A hook script is provided to lookup the hostname if not set by
  #  # the DHCP server, but it should not be run by default.
  #  nohook lookup-hostname

  #  # Ignore peth* devices; on Xen, they're renamed physical
  #  # Ethernet cards used for bridging.  Likewise for vif* and tap*
  #  # (Xen) and virbr* and vnet* (libvirt).
  #  denyinterfaces ve-* vb-* lo peth* vif* tap* tun* virbr* vnet* vboxnet* sit*

  #  # Use the list of allowed interfaces if specified


  #  # Immediately fork to background if specified, otherwise wait for IP address to be assigned
  #  waitip

  #  option host_name
  #'';

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkb.options in tty.
  };

  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (action.id.indexOf("org.freedesktop.NetworkManager.") == 0 && subject.isInGroup("network")) {
        return polkit.Result.YES;
      }
    });
  '';

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  services.desktopManager.plasma6.enable = true;
  services.displayManager = {
    enable = true;
    sddm.enable = true;
    sddm.wayland.enable = true;
  };

  # Configure keymap in X11
  services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  users.users.hexular = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    shell = pkgs.zsh;
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
    git
    brightnessctl
    networkmanagerapplet
    spotify
    ghidra
    wireshark
    gdb
    gcc
    bcc
    bpftrace
    kdePackages.kdenlive
    lm_sensors
    krita
    # unityhub
    dotnet-sdk
    csharp-ls
  ];

  # services.tailscale = {
  #   enable = true;
  # };

  programs.sway.enable = true;

  networking.firewall.enable = false;

  systemd.oomd.enable = true;

  services.tzupdate = {
    enable = true;
    timer.enable = true;
  };

  programs.steam.enable = true;

  hardware.bluetooth.enable = true;

  virtualisation.docker = {
      enable = true;
  };

  home-manager.extraSpecialArgs = let
      homeDir = config.users.users.hexular.home;
  in {
      inherit inputs homeDir;
      configDir = "${homeDir}/config";
  };
  home-manager.users.hexular = { pkgs, ... }: {
      imports = [
          ./home.nix
      ];
  };

  system.stateVersion = "24.05";

}

