{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    zip
    tree
    unzip
    brave
    google-chrome
    nodejs_22
    pnpm
    yarn
    supabase-cli
    docker-compose
    opencode
  ];

  programs.nix-ld.enable = true;

  virtualisation.docker.enable = true;
  users.users.max.extraGroups = [ "docker" ];
}
