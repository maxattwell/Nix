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
    claude-code
    opencode
  ];

  virtualisation.docker.enable = true;
  users.users.max.extraGroups = [ "docker" ];
}
