{ config, pkgs, lib, modulesPath, ... }:

{

  imports = [
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
  ];

  sdImage = {
    populateRootCommands = ''

      mkdir -p ./files/etc/nixos
      cp -r ${./.}/* ./files/etc/nixos/
    '';
    compressImage = false;
    expandOnBoot = true;
  };

}
