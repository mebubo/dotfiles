{ config, pkgs, lib, ... }:

{
  services.home-assistant = {
    enable = true;
    extraComponents = [
      "default_config"
      "met"
      "esphome"
      "radio_browser"
      "generic_thermostat"
      "mqtt"
    ];
    config = {
      default_config = {};
      mqtt = {
        broker = "localhost";
        port = 1883;
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 8123 ];
}
