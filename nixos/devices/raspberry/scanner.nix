{ config, pkgs, ... }:


{

  hardware.sane = {
    enable = true;
    # extraBackends = [ pkgs.sane-airscan ];
    extraBackends = [ pkgs.hplipWithPlugin ];
  };

  services.ipp-usb.enable = true;

  users.users.me = {
    extraGroups = [ "scanner" ];
  };
}
