{ config, lib, pkgs, ... }:

{
  boot.kernelParams = [
    "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
    "nvidia.NVreg_TemporaryFilePath=/var/tmp"
    "nvidia-drm.modeset=1"
    "nvidia-drm.fbdev=1"
  ];

  # Enable NVIDIA suspend services
  systemd.services = {
    nvidia-suspend.enable = true;
    nvidia-hibernate.enable = true;
    nvidia-resume.enable = true;
  };

  hardware.graphics.enable = true;

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  environment.systemPackages = with pkgs; [
    nvtopPackages.nvidia
  ];
}
