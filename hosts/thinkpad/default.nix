{ ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/profiles/desktop-environment.nix
    ../../modules/nixos/profiles/laptop.nix
    ../../modules/nixos/profiles/zram.nix
    ../../modules/nixos/dns-public.nix
    ../../modules/nixos/profiles/emacs.nix
    ../../modules/nixos/profiles/dev.nix
    ../../modules/nixos/profiles/insurgence.nix
    ../../modules/nixos/profiles/remote-access.nix
  ];

  system.stateVersion = "26.05"; # Did you read the comment?

  services.fprintd.enable = true;

  services.tlp = {
    enable = true;
    settings = {
      START_CHARGE_THRESH_BAT0 = 75;
      STOP_CHARGE_THRESH_BAT0 = 90;

      PLATFORM_PROFILE_ON_AC = "performance";
  
      PCIE_ASPM_ON_BAT = "powersupersave";
    };
  };
}
