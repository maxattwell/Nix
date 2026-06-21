{ config, lib, pkgs, hostname, ... }:

{
  imports = [
    ../nix-cache.nix
  ];

  networking.hostName = hostname;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Keep package documentation outputs out of the system closure.
  # This also avoids nixos-unstable python3.12-3.12.13-doc
  # Sphinx/docutils build failures pulled in via dev Python packages.
  # See NixOS/nixpkgs#499166 and #529084.
  documentation.doc.enable = false;

  users.users.max = {
    isNormalUser = true;
    description = "Max Attwell";
    extraGroups = [ "wheel" ];
    packages = with pkgs; [];
    shell = pkgs.zsh;
  };

  programs.zsh.enable = true;

  services.envfs.enable = true; # Maps standard paths like /bin/bash to the Nix store for script compatibility

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    git
    neovim
    nitch
    nh
    pass
    gnupg
  ];

  services.logind = {
    settings.Login = {
      KillUserProcesses = false;
    };
  };

  # Locale settings
  time.timeZone = "Pacific/Auckland";

  i18n.defaultLocale = "en_NZ.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_NZ.UTF-8";
    LC_IDENTIFICATION = "en_NZ.UTF-8";
    LC_MEASUREMENT = "en_NZ.UTF-8";
    LC_MONETARY = "en_NZ.UTF-8";
    LC_NAME = "en_NZ.UTF-8";
    LC_NUMERIC = "en_NZ.UTF-8";
    LC_PAPER = "en_NZ.UTF-8";
    LC_TELEPHONE = "en_NZ.UTF-8";
    LC_TIME = "en_NZ.UTF-8";
  };

  services.xserver.xkb = {
    layout = "nz";
  };

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

}
