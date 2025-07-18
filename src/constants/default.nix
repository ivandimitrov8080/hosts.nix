{
  nova =
    { lib, modulesPath, ... }:
    {
      imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];
      boot = {
        initrd = {
          availableKernelModules = [
            "xhci_pci"
            "thunderbolt"
            "nvme"
            "usb_storage"
            "sd_mod"
            "sdhci_pci"
          ];
          kernelModules = [ ];
          luks.devices."nixos".device = "/dev/disk/by-uuid/712dd8ba-d5b4-438a-9a77-663b8c935cfe";
        };
        kernelModules = [ "kvm-intel" ];
        extraModulePackages = [ ];
      };
      fileSystems = {
        "/" = {
          device = "/dev/disk/by-uuid/47536cbe-7265-493b-a2e3-bbd376a6f9af";
          fsType = "btrfs";
        };
        "/boot" = {
          device = "/dev/disk/by-uuid/4C3C-993A";
          fsType = "vfat";
          options = [
            "uid=0"
            "gid=0"
            "fmask=0077"
            "dmask=0077"
          ];
        };
      };
      swapDevices = [ ];
      networking.useNetworkd = lib.mkDefault true;
      nixpkgs.hostPlatform = lib.mkForce "x86_64-linux";
      hardware.cpu.intel.updateMicrocode = lib.mkForce false;
    };
  stara =
    {
      lib,
      modulesPath,
      config,
      ...
    }:
    {
      imports = [
        (modulesPath + "/installer/scan/not-detected.nix")
      ];

      boot.initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "usb_storage"
        "sd_mod"
      ];
      boot.initrd.kernelModules = [ ];
      boot.kernelModules = [ "kvm-intel" ];
      boot.extraModulePackages = [ ];

      fileSystems."/" = {
        device = "/dev/disk/by-uuid/23cab329-d467-45d1-acc4-8bf43958c1ab";
        fsType = "btrfs";
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/ACB8-8C22";
        fsType = "vfat";
        options = [
          "fmask=0022"
          "dmask=0022"
        ];
      };

      networking.useNetworkd = lib.mkDefault true;

      swapDevices = [ ];

      nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
      hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    };
}
