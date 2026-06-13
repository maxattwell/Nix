{ ... }:

{
  networking = {
    # Avoid flaky router/DHCP DNS by using explicit public resolvers.
    networkmanager.dns = "none";
    nameservers = [ "1.1.1.1" "8.8.8.8" ];
  };
}
