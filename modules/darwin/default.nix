{ config, lib, pkgs, ... }:

{
  nix.settings.experimental-features = "nix-command flakes";

  users.users.max = {
    home = "/Users/max";
    shell = pkgs.zsh;
  };

  environment.systemPackages = with pkgs; [
    emacs-29
    nodejs_22
    docker
    colima
    pnpm
    yarn
    ripgrep
    brave
    supabase-cli
    watchman
    # Java OpenJDK
    zulu17
    gnupg
    pass
    flyctl
    go
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-29;
  };

  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  system.defaults.dock.autohide = true;
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;
}
