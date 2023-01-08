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

    swapDevices = [ 
        { device = "/dev/disk/by-uuid/1691f745-a841-4ac6-a3cf-d03397cf3b21"; }
        { device = "/dev/disk/by-uuid/14a3f709-666d-4cec-8693-f88780f1b6aa"; }
    ];

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
