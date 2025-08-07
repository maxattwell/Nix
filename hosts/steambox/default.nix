{ config, pkgs, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
  ];

  networking = {
    hostName = "steambox";
    networkmanager.enable = true;
    useNetworkd = false;
    firewall.enable = false;
  };

  environment.etc."resolv.conf".text = ''
    nameserver 1.1.1.1
    nameserver 8.8.8.8
  '';


  # KDE Plasma Desktop Environment
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;

  # NVIDIA configuration for gaming
  boot.kernelParams = [
    "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
    "nvidia.NVreg_TemporaryFilePath=/var/tmp"
  ];

  systemd.services = {
    nvidia-suspend.enable = true;
    nvidia-hibernate.enable = true;
    nvidia-resume.enable = true;
  };

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  # Audio configuration for gaming and streaming
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  environment.systemPackages = with pkgs; [
    git
    zip
    neovim
    tree
    unzip
    bluez
    blueman
    btop
    nvtopPackages.nvidia
    nh

    # Media and utilities
    vlc
    ffmpeg
    brave
    google-chrome
  ];

  services.openssh.enable = true;

  system.stateVersion = "24.11";
}
