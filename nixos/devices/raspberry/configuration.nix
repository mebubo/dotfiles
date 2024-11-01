{ ... }:

{

  imports = [
    ./sd-image.nix
    ./minimal.nix
    ./minimal-networking.nix
    ./users.nix
    ./zigbee.nix
    # ./router.nix
    # ./desktop.nix
    ./printing.nix
    ./scanner.nix
    # ./wireless.nix
    ./rpi-eeprom-update.nix
  ];

}
