{ pkgs, ... }:

let

  script = pkgs.writeShellScriptBin "rpi-eeprom-update" ''
    set -exu
    mount /dev/disk/by-label/FIRMWARE /mnt
    BOOTFS=/mnt FIRMWARE_RELEASE_STATUS=stable ${pkgs.raspberrypi-eeprom}/bin/rpi-eeprom-update -d -a
  '';

in

{

  environment.systemPackages = [
    pkgs.raspberrypi-eeprom
    script
  ];
}
