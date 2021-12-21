{ config, lib, pkgs, modulesPath, ... }: {
    imports = [ 
        (modulesPath + "/installer/scan/not-detected.nix")
    ];

    boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "sd_mod" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-amd" ];
    boot.extraModulePackages = [ ];

    fileSystems."/" = { 
        device = "/dev/disk/by-uuid/86ca218b-3aee-4783-9d0e-2c6bd616d564";
        fsType = "ext4";
    };

    fileSystems."/boot" = { 
        device = "/dev/disk/by-uuid/16E5-542D";
        fsType = "vfat";
    };

    fileSystems."/hdd" = {
        device = "/dev/disk/by-uuid/6494f0d3-4b3e-4d06-8080-8446a302ad93";
        fsType = "ext4";
    };

    swapDevices = [ 
        { device = "/dev/disk/by-uuid/14a3f709-666d-4cec-8693-f88780f1b6aa"; }
    ];

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
