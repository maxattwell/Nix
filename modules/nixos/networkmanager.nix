{ ... }:

{
  networking = {
    networkmanager.enable = true;
    useNetworkd = false;
  };

  users.users.max.extraGroups = [ "networkmanager" ];
}
