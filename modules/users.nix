{ config, lib, pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.max = {
    isNormalUser = true;
    description = "Max Attwell";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
  };
}
