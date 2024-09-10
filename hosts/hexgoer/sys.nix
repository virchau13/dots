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
      availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" ];
      kernelModules = [ ];
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    kernelParams = [ "boot.shell_on_fail" "acpi_backlight=intel" ];
    # enable SysRq key
    kernel.sysctl."kernel.sysrq" = 1;
  
};

  networking.hostName = "hexgoer";
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkb.options in tty.
  };

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

  # Enable sound.
  # hardware.pulseaudio.enable = true;
  # OR
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.hexular = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
    git
  ];

  programs.sway.enable = true;

  networking.firewall.enable = false;

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

