{ config, pkgs, ... }:

let

device-number = "10";
device-label = "MyCamera";

in

{
  boot = {
    extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
    extraModprobeConfig = ''options v4l2loopback exclusive_caps=1 video_nr=${device-number} card_label=${device-label}'';
    kernelModules = [ "v4l2loopback" ];
  };

  nixpkgs.overlays = [
    (self: super: {

      wf-recorder = super.wf-recorder.overrideAttrs (attrs: {
        version = "dev";
        src = self.fetchFromGitHub {
          owner = "ammen99";
          repo = "wf-recorder";
          rev = "bb7aff7a52598b70b87da351822e8e58caa1a973";
          sha256 = "sha256-5EoeCZf7t8esbrskvusaEdoOyV1HCzmescQ0oH5wE70=";
        };
      });

      screen-share = self.writeShellScriptBin "screen-share" ''
        ${self.wf-recorder}/bin/wf-recorder --file=/dev/video${device-number} --muxer=v4l2 --pixel-format yuv420p --codec=rawvideo
      '';

    })
  ];

  environment.defaultPackages = [ pkgs.screen-share ];

}
