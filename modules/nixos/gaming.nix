{ pkgs, lib, config, ... }:

{
  # Steam configuration with remote play support
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
    gamescopeSession.enable = true;
  };

  # Gaming performance optimizations
  programs.gamemode.enable = true;

  # Hardware acceleration for gaming
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  # Allow unfree packages for gaming
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "steam"
    "steam-original"
    "steam-unwrapped"
    "steam-run"
    "nvidia-x11"
    "nvidia-settings"
  ];

  # Gaming and streaming packages
  environment.systemPackages = with pkgs; [
    # Core gaming
    steam
    lutris
    heroic
    bottles
    
    # Performance monitoring and tools
    mangohud
    goverlay
    gamemode
    
    # Game streaming
    sunshine
    moonlight-qt
    
    # Controller support
    dualsensectl
    antimicrox
    
    # Audio/Video tools for streaming
    obs-studio
    v4l-utils
    
    # Compatibility tools
    protonup-qt
    protontricks
    winetricks
    
    # System monitoring
    btop
    nvtopPackages.nvidia
  ];

  # Sunshine game streaming service
  services.sunshine = {
    enable = true;
    openFirewall = true;
    capSysAdmin = true;
  };

  # Controller support
  hardware.steam-hardware.enable = true;
  
  # Udev rules for controllers
  services.udev.packages = with pkgs; [
    dualsensectl
  ];

  # Gaming-optimized kernel parameters
  boot.kernel.sysctl = {
    "vm.max_map_count" = 2147483642;
  };

  # Performance tweaks
  powerManagement.cpuFreqGovernor = "performance";
  
  # Network optimizations for gaming
  boot.kernelModules = [ "tcp_bbr" ];
  boot.kernel.sysctl."net.ipv4.tcp_congestion_control" = "bbr";
  boot.kernel.sysctl."net.core.default_qdisc" = "fq";
}