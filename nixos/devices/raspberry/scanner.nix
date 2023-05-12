{ config, pkgs, ... }:

let

  scan-pdf = pkgs.writeShellScriptBin "scan-pdf" ''
    ${pkgs.sane-backends}/bin/scanimage -x 210 --format=pdf --resolution 300 --mode Color --batch=$1.pdf --batch-prompt --contrast 300
  '';
in

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

  environment.systemPackages = with pkgs; [
    scan-pdf
  ];
}
