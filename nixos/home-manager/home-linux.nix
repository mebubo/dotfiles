{ config, pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    binutils
    libinput
    usbutils
    v4l-utils
    android-tools
  ];

  # targets.genericLinux.enable = true;
}
