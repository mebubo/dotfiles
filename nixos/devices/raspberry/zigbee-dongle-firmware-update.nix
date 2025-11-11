{ fetchFromGitHub, python3Packages, python3, unzip, runCommand, writeShellScriptBin }:

let

  pname = "cc2538-bsl";

  src = fetchFromGitHub {
    owner = "JelmerT";
    repo = pname;
    rev = "538ea0deb99530e28fdf1b454e9c9d79d85a3970";
    sha256 = "sha256-fPY12kValxbJORi9xNyxzwkGpD9F9u3M1+aa9IlSiaE=";
  };

  py = python3.withPackages (p: [ p.pyserial p.intelhex ]);

  firmware = builtins.fetchurl {
    url = "https://github.com/Koenkk/Z-Stack-firmware/releases/download/Z-Stack_3.x.0_coordinator_20250321/CC1352P2_CC2652P_launchpad_coordinator_20250321.zip";
    sha256 = "sha256:0fi70idxjjxl04dlsskk0zbkjpjjb6wlfsns6d8kg99c1k9d7xb6";
  };

  firmware-unpacked = runCommand "firmare-unpack" {} ''
    mkdir $out
    cd $out
    ${unzip}/bin/unzip ${firmware}
  '';

in

  writeShellScriptBin "zigbee-dongle-firmware-update" ''
    ${py}/bin/python ${src}/cc2538-bsl.py -ewv -p /dev/ttyUSB0 --bootloader-sonoff-usb ${firmware-unpacked}/CC1352P2_CC2652P_launchpad_coordinator_20250321.hex
  ''
