{ config, lib, pkgs, ... }:

{
  #services.xserver.windowManager.i3.enable = true;

  services.xserver.enable = true;

  services.xserver.desktopManager.kodi = {
    enable = true;
    package = pkgs.kodi.withPackages (
      p: with p; [
        joystick
        inputstream-adaptive
        trakt
        vfs-rar
        vfs-libarchive
      ]
    );
  };
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "kodi";
  services.xserver.displayManager.lightdm.greeter.enable = false;

  # Define a user account
  users.extraUsers.kodi = {
    isNormalUser = true;
    password = "kodi";
  };

  # Allow wake on LAN
  networking.interfaces."enp2s0".wakeOnLan.enable = true;

  # Open ports in the firewall.
  networking.firewall = {
    allowedTCPPorts = [ 8080 ];
    allowedUDPPorts = [ 8080 ];
  };
}
