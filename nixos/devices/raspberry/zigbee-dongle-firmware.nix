let
  pkgs = import <nixpkgs> {};

  pname = "cc2538-bsl";

  src = pkgs.fetchFromGitHub {
    owner = "JelmerT";
    repo = pname;
    rev = "538ea0deb99530e28fdf1b454e9c9d79d85a3970";
    sha256 = "sha256-fPY12kValxbJORi9xNyxzwkGpD9F9u3M1+aa9IlSiaE=";
  };


  app = pkgs.python3Packages.buildPythonApplication {
    version = "2.1";
    nativeBuildInputs = [ pkgs.python3Packages.setuptools-scm ];
    propagatedBuildInputs = [ pkgs.python3Packages.setuptools ];
    inherit src pname;
  };

  py = pkgs.python3.withPackages (p: [ p.pyserial p.intelhex ]);

  firmware = builtins.fetchurl {
    url = "https://github.com/Koenkk/Z-Stack-firmware/raw/master/coordinator/Z-Stack_3.x.0/bin/CC1352P2_CC2652P_launchpad_coordinator_20221226.zip";
    sha256 = "sha256:1566hzdf7a4687iy8wk0qrg6g29qki0k5k8r8vin39j82ijq7n1n";
  };

  firmware-unpacked = pkgs.runCommand "firmare-unpack" {} ''
    mkdir $out
    cd $out
    ${pkgs.unzip}/bin/unzip ${firmware}
  '';

in

  pkgs.writeShellScriptBin "flash" ''
    sha256sum ${firmware-unpacked}/*
    # ${py}/bin/python ${src}/cc2538-bsl.py -ewv -p /dev/ttyUSB0 --bootloader-sonoff-usb ${firmware-unpacked}/CC1352P2_CC2652P_launchpad_coordinator_20221226.hex
    ${py}/bin/python ${src}/cc2538-bsl.py $@
  ''
