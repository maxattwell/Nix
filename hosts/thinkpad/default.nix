{ ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/profiles/desktop-environment.nix
    ../../modules/nixos/profiles/laptop.nix
    ../../modules/nixos/profiles/zram.nix
    ../../modules/nixos/profiles/emacs.nix
    ../../modules/nixos/profiles/dev.nix
    ../../modules/nixos/profiles/insurgence.nix
    ../../modules/nixos/profiles/remote-access.nix
  ];

  services.resolved.enable = true;
  networking.networkmanager.dns = "systemd-resolved";

  boot.resumeDevice = "/dev/disk/by-uuid/891fd660-0886-48a8-a54a-76d17d937268";

  services.tlp = {
    enable = true;
    settings = {
      START_CHARGE_THRESH_BAT0 = 75;
      STOP_CHARGE_THRESH_BAT0 = 90;

      PLATFORM_PROFILE_ON_AC = "performance";
  
      PCIE_ASPM_ON_BAT = "powersupersave";
    };
  };

  services.logind.settings.Login = {
    HandleLidSwitch = "suspend-then-hibernate";
    HandlePowerKey = "suspend-then-hibernate";
  };
  
  systemd.sleep.settings.Sleep = {
    HibernateDelaySec = "40m";
    SuspendState = "mem";
  };

  # Moonlight client for streaming from nixbox's Sunshine host.
  programs.moonlight-qt = {
    enable = true;
    # Allow raising the scheduling priority for smoother frame pacing.
    capSysNice = true;
  };

  # Sunshine encodes full/JPEG color range; Moonlight's Linux decoder
  # otherwise assumes limited range and washes the colors out.
  environment.sessionVariables.COLOR_RANGE_OVERRIDE = "2";

  system.stateVersion = "26.05"; # Did you read the comment?
}
