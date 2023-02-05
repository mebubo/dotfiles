{ pkgs, ... }:

let

  zigbee-dongle-firmware-update = pkgs.callPackage ./zigbee-dongle-firmware-update.nix {};

in

{

  services = {
    zigbee2mqtt = {
      enable = true;
      settings = {
        serial = {
          port = "/dev/ttyUSB0";
        };
        frontend = {
          host = "0.0.0.0";
          port = 8080;
        };
        permit_join = false;
      };
    };
    mosquitto = {
      enable = true;
      listeners = [];
    };

    telegraf = {
      enable = true;
      extraConfig = {
        inputs.mqtt_consumer = {
          servers = [ "tcp://127.0.0.1:1883" ];
          topics = [ "zigbee2mqtt/+" ];
          data_format = "json";
        };
        outputs.influxdb_v2 = {
          urls = [ "http://localhost:8086" ];
          organization = "me";
          bucket = "me";
          token = "";
        };
      };
    };

    influxdb2 = {
      enable = true;
    };
  };

  networking.firewall.allowedTCPPorts = [ 8080 ];

  environment.systemPackages = with pkgs; [
    jq
    mosquitto
    influxdb2-cli
    zigbee-dongle-firmware-update
  ];

}
