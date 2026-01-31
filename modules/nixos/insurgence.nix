{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    teams-for-linux

    azure-cli

    dbeaver-bin

    cacert

    cloudflare-warp
    cloudflared  # Add cloudflared for tunnels
    docker-buildx
  ];

  # Cloudflare WARP - Zero Trust Client
  services.cloudflare-warp.enable = true;
  services.cloudflare-warp.openFirewall = true;
}
