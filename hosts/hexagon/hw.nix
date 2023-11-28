{ config, lib, pkgs, modulesPath, ... }: {
    imports = [ 
        (modulesPath + "/installer/scan/not-detected.nix")
    ];

    boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "sd_mod" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-amd" ];
    boot.extraModulePackages = [ ];

# TODO change
    fileSystems."/" = { 
        device = "/dev/disk/by-uuid/f0ce0357-daf2-47a8-97cc-b5ae428441df";
        fsType = "xfs";
    };

    fileSystems."/boot" = { 
        device = "/dev/disk/by-uuid/A774-6C06";
        fsType = "vfat";
    };

    # just died on me for no reason lmfao
    # fileSystems."/hdd" = {
    #     device = "/dev/disk/by-uuid/6494f0d3-4b3e-4d06-8080-8446a302ad93";
    #     fsType = "ext4";
    # };

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
