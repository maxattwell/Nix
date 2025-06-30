{ config, pkgs, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/sddm.nix
  ];

  networking = {
    hostName = "nixbox";
    networkmanager.enable = true;
    useNetworkd = false;
    firewall.enable = false;
  };

  environment.etc."resolv.conf".text = ''
    nameserver 1.1.1.1
    nameserver 8.8.8.8
  '';


  services.logind = {
    killUserProcesses = false;
    lidSwitch = "suspend";
    powerKey = "suspend";
  };

  # services.displayManager.ly.enable = true;

  boot.kernelParams = [
    "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
    "nvidia.NVreg_TemporaryFilePath=/var/tmp"
  ];

  # Enable NVIDIA suspend services
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

  hardware.bluetooth.enable = true;

  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    git
    zip
    neovim
    nitch
    tree
    unzip
    bluez
    blueman
    btop
    nvtopPackages.nvidia
    nh

    pass
    gnupg
    ripgrep
    ispell
    wofi
    vlc
    slurp
    grim
    wf-recorder
    ffmpeg
    kitty
    emacs
    brave
    google-chrome

    nodejs_22
    pnpm
    yarn
    supabase-cli
    docker-compose
    rustc
    cargo
    maturin
    python3Full
    uv
    go
    awscli2

    vscode
    inputs.opencode-flake.packages.${pkgs.system}.default
  ];

  services.openssh.enable = true;

  # services.ollama = {
  #   enable = true;
  #   acceleration = "cuda";
  # };

  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  virtualisation.docker.enable = true;
  users.users.max.extraGroups = [ "docker" ];

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
