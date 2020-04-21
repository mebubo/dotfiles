{ config, lib, pkgs, ... }:

let

  external-display-hotplug = pkgs.writeShellScript "external-display-hotplug" ''
STATUS_FILE=/sys/class/drm/card0-DP-1/status
STATUS=$(${pkgs.coreutils}/bin/cat $STATUS_FILE)
PULSE_SERVER="unix:/run/user/$UID/pulse/native"
export SWAYSOCK=$(${pkgs.coreutils}/bin/readlink -f /run/user/1000/sway-ipc.1000.*.sock)

env >> /tmp/h

case $STATUS in
  disconnected)
    ${pkgs.sway}/bin/swaymsg output LVDS-1 enable
    ${pkgs.sway}/bin/swaymsg output DP-1 disable
    ${pkgs.pulseaudio}/bin/pactl --server "$PULSE_SERVER" set-card-profile 0 output:analog-stereo+input:analog-stereo
    ;;
  connected)
    ${pkgs.sway}/bin/swaymsg output DP-1 enable
    ${pkgs.sway}/bin/swaymsg output LVDS-1 disable
    ${pkgs.pulseaudio}/bin/pactl --server "$PULSE_SERVER" set-card-profile 0 output:hdmi-stereo
    ;;
  esac
'';

in
{
  boot.kernelParams = [ ''acpi_osi="!Windows 2012"'' "i915.enable_rc6=7" ];

  boot.initrd.luks.devices."root" = {
    allowDiscards = true;
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="rfkill", ATTR{type}=="bluetooth", ATTR{state}="0"
    ACTION=="change", SUBSYSTEM=="drm", ENV{HOTPLUG}=="1", RUN+="${pkgs.sudo}/bin/sudo -u me ${external-display-hotplug}"
  '';

  hardware.cpu.intel.updateMicrocode = true;

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
    ];
  };

  security.allowSimultaneousMultithreading = false;
}
