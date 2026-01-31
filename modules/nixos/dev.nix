{ config, lib, pkgs, ... }:

{
  imports = [
    ./insurgence.nix
  ];

  environment.systemPackages = with pkgs; [
    zip
    tree
    unzip
    brave
    google-chrome
    nodejs_22
    bun
    pnpm
    yarn
    supabase-cli
    docker-compose

    uv
    python312
    python312Packages.uvicorn

    postgresql
    pre-commit

    mitmproxy
  ];

  # Enable gpu-screen-recorder with proper permissions
  security.wrappers.gsr-kms-server = {
    owner = "root";
    group = "root";
    capabilities = "cap_sys_admin+ep";
    source = "${pkgs.gpu-screen-recorder}/bin/gsr-kms-server";
  };

  programs.nix-ld.enable = true;

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;  # Start Docker only when needed via socket activation
  };
  users.users.max.extraGroups = [ "docker" ];
}
