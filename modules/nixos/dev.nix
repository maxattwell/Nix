{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    nodejs_22
    pnpm
    yarn
    supabase-cli
    docker-compose
    opencode
  ];
}
