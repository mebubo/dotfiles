{ config, pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    binutils
    libinput
    usbutils
    v4l-utils
  ];

  # targets.genericLinux.enable = true;
}
