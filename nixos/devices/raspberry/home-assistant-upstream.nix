{ config, pkgs, lib, ... }:

{
  virtualisation.podman.enable = true;

  users.users.podman-home-assistant = {
    isSystemUser = true;
    group = "podman-home-assistant";
    description = "Home Assistant (rootless Podman)";
    home = "/var/lib/podman-home-assistant";
    createHome = true;
    # required for rootless user namespaces
    subUidRanges = [{ startUid = 100000; count = 65536; }];
    subGidRanges = [{ startGid = 100000; count = 65536; }];
  };
  users.groups.podman-home-assistant = {};

  virtualisation.oci-containers = {
    backend = "podman";
    containers.homeassistant = {
      volumes = [ "home-assistant:/config" ];
      environment.TZ = "Europe/Paris";
      # Note: The image will not be updated on rebuilds, unless the version label changes
      image = "ghcr.io/home-assistant/home-assistant:stable";
      extraOptions = [
        "--network=host"
      ];
      podman.user = "podman-home-assistant";
    };
  };
  networking.firewall.allowedTCPPorts = [ 8123 ];
}
