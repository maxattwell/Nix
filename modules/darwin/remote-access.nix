{ config, lib, pkgs, ... }:

{
  services.openssh = {
    enable = true;
    extraConfig = ''
      PasswordAuthentication no
      KbdInteractiveAuthentication no
      PermitRootLogin no
    '';
  };

  services.tailscale.enable = true;
}
