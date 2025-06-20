{ config, pkgs, ... }:

{
  # Enable Docker
  virtualisation.docker.enable = true;

  # Add your user to docker group (replace 'username' with your actual username)
  users.users.username.extraGroups = [ "docker" ];

  # Open required ports
  networking.firewall.allowedTCPPorts = [
    80    # Pi-hole web interface
    443   # Pi-hole web interface (HTTPS)
    53    # DNS
  ];
  networking.firewall.allowedUDPPorts = [
    53    # DNS
    67    # DHCP (if you plan to use Pi-hole as DHCP server)
  ];

  # Optional: Disable systemd-resolved to avoid port 53 conflicts
  services.resolved.enable = false;

  # Create systemd service for Pi-hole
  systemd.services.pihole = {
    description = "Pi-hole DNS Ad Blocker";
    after = [ "docker.service" "docker.socket" ];
    requires = [ "docker.service" "docker.socket" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.docker}/bin/docker run -d --name pihole " +
        "--restart=unless-stopped " +
        "-p 53:53/tcp -p 53:53/udp " +
        "-p 80:80/tcp " +
        "-p 443:443/tcp " +
        "-e TZ='America/New_York' " +  # Change to your timezone
        "-e WEBPASSWORD='your-secure-password' " +  # Change this!
        "-e SERVERIP='192.168.1.100' " +  # Your NixOS server IP
        "-v /var/lib/pihole/etc-pihole:/etc/pihole " +
        "-v /var/lib/pihole/etc-dnsmasq.d:/etc/dnsmasq.d " +
        "--dns=1.1.1.1 --dns=1.0.0.1 " +
        "--cap-add=NET_ADMIN " +
        "pihole/pihole:latest";

      ExecStop = "${pkgs.docker}/bin/docker stop pihole";
      ExecStopPost = "${pkgs.docker}/bin/docker rm -f pihole";
    };
  };

  # Create directories for Pi-hole data
  systemd.tmpfiles.rules = [
    "d /var/lib/pihole 0755 root root -"
    "d /var/lib/pihole/etc-pihole 0755 root root -"
    "d /var/lib/pihole/etc-dnsmasq.d 0755 root root -"
  ];
}
