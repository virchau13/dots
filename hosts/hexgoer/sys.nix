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
  networking.networkmanager.enable = true;

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
    extraGroups = [ "wheel" "network" ];
    shell = pkgs.zsh;
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
    git
    brightnessctl
    networkmanagerapplet
    spotify
    kdenlive
    ghidra
    wireshark
    gdb
    gcc
    bcc
    bpftrace
  ];

  programs.sway.enable = true;

  networking.firewall.enable = false;

  services.tzupdate.enable = true;

  programs.steam.enable = true;

  hardware.bluetooth.enable = true;

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

