{ config, lib, pkgs, inputs, ... }:

{
  nix.settings.experimental-features = "nix-command flakes";

  users.users.max = {
    home = "/Users/max";
    shell = pkgs.zsh;
  };

  system.primaryUser = "max";

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    nh
    neovim
    gnupg
    pass

    kitty

    brave

    nodejs_22
    pnpm
    yarn
    supabase-cli
    watchman
    # Java OpenJDK
    zulu17
    eas-cli
    claude-code
    opencode
  ];

  system.defaults.dock.autohide = true;
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;
}
