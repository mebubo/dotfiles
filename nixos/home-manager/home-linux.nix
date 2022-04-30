{ config, pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    binutils
    libinput
    usbutils
    v4l_utils
  ];

  targets.genericLinux.enable = true;
}
