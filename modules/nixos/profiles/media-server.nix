{ pkgs, ... }:

{
  imports = [
    ../nixarr.nix
    ../homepage.nix
    ../calibre-web.nix
  ];

  networking.nameservers = [ "8.8.8.8" "1.1.1.1" ];

  environment.systemPackages = with pkgs; [
    ghostty.terminfo
  ];

  virtualisation.docker = {
    enable = true;
    daemon.settings = {
      dns = [ "8.8.8.8" "1.1.1.1" ];
    };
  };

  systemd.services.docker.environment = {
    GODEBUG = "netdns=cgo";
  };

  users.users.max.extraGroups = [ "docker" ];
}
