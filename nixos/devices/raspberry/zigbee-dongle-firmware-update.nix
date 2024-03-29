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
    url = "https://github.com/Koenkk/Z-Stack-firmware/raw/master/coordinator/Z-Stack_3.x.0/bin/CC1352P2_CC2652P_launchpad_coordinator_20230507.zip";
    sha256 = "sha256:1d6f667ykrlz7077b6yy3wm6cxd0fyaqdhvbq7acpdm8dhxjwivq";
  };

  firmware-unpacked = runCommand "firmare-unpack" {} ''
    mkdir $out
    cd $out
    ${unzip}/bin/unzip ${firmware}
  '';

in

  writeShellScriptBin "zigbee-dongle-firmware-update" ''
    ${py}/bin/python ${src}/cc2538-bsl.py -ewv -p /dev/ttyUSB0 --bootloader-sonoff-usb ${firmware-unpacked}/CC1352P2_CC2652P_launchpad_coordinator_20230507.hex
  ''
