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


  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
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
    bluez
    blueman
    btop
    nvtopPackages.nvidia

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

    nodejs_22
    pnpm
    yarn
    supabase-cli
    rustc
    cargo
    maturin
    python3Full
    uv
    go

    aider-chat
    code-cursor
    codex
    claude-code
  ];

  services.openssh.enable = true;

  services.emacs.enable = true;

  services.ollama = {
    enable = true;
    acceleration = "cuda";
  };

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
