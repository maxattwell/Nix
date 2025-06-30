{ config, lib, pkgs, inputs, ... }:

{
  nix.settings.experimental-features = "nix-command flakes";

  users.users.max = {
    home = "/Users/max";
    shell = pkgs.zsh;
  };

  system.primaryUser = "max";

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    nh
    neovim
    emacs-30
    kitty
    ispell
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
    eas-cli
    aider-chat
    claude-code
    goose-cli
    # opencode
    vscode
    inputs.opencode-flake.packages.${pkgs.system}.default
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-30;
  };

  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  system.defaults.dock.autohide = true;
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;
}
