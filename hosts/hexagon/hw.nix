{ config, lib, pkgs, modulesPath, ... }: {
    imports = [ 
        (modulesPath + "/installer/scan/not-detected.nix")
    ];

    boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "sd_mod" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-amd" ];
    boot.extraModulePackages = [ ];

    fileSystems."/" = { 
        device = "/dev/disk/by-uuid/6851773a-37ab-45d7-93c4-8981c0ceabb2";
        fsType = "xfs";
    };

    fileSystems."/boot" = { 
        device = "/dev/disk/by-uuid/7341-93CB";
        fsType = "vfat";
    };
}
