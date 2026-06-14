{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    tree
    nodejs
    pnpm
    supabase-cli
    docker-compose
    uv
    python312
    python312Packages.uvicorn
    postgresql
    pre-commit
    mitmproxy
  ];

  programs.nix-ld.enable = true;

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;  # Start Docker only when needed via socket activation
  };

  users.users.max.extraGroups = [ "docker" ];
}
