{ config, pkgs, ... }:

let

  printer = (import ./private.nix).printer;

  printer-setup = pkgs.writeShellScriptBin "printer-setup" ''
    ${pkgs.cups}/bin/lpadmin -p lj2 -E -v ${printer.uri} -m ${printer.model} -o printer-is-shared=true
    ${pkgs.cups}/bin/lpadmin -d lj2
  '';

  # hplip = pkgs.hplip { withQt5 = false; withPlugin = true; };
  hplip = pkgs.hplipWithPlugin;

in

{
  environment.systemPackages = with pkgs; [
    printer-setup
  ] ;

  nixpkgs.config.allowUnfree = true;

  networking.firewall.allowedTCPPorts = [ 631 ];
  networking.firewall.allowedUDPPorts = [ 631 ];

  services.printing = {
    enable = true;
    # drivers = [ (pkgs.hplipWithPlugin_3_18_5.override { withQt5 = false; }) ];
    # drivers = [ (pkgs.hplipWithPlugin.override { withQt5 = false; }) ];
    drivers = [ hplip ];
    browsing = true;
    listenAddresses = [ "*:631" ];
    defaultShared = true;
    allowFrom = [ "localhost" "192.168.1.*" ];
    logLevel = "debug";
    extraConf = pkgs.lib.mkOverride 0 ''
      <Location />
        Order allow,deny
        Allow all
      </Location>
'';
  };

  hardware.printers = {
    ensureDefaultPrinter = "hp";
    ensurePrinters = [
      {
        deviceUri = printer.uri;
        model = printer.model;
        name = "hp";
      }
    ];
  };

  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      userServices = true;
    };
  };

}
