# based on https://github.com/NixOS/nixpkgs/blob/5b3321b4e829887e08c5c49dd1e4fc1b38d6b588/pkgs/development/compilers/jetbrains-jdk/default.nix

{ lib
, stdenv
, fetchFromGitHub
, jetbrains
, openjdk17
}:

openjdk17.overrideAttrs (oldAttrs: rec {
  pname = "jetbrains-jdk";
  version = "17.0.5-b653.14";

  src = fetchFromGitHub {
    owner = "JetBrains";
    repo = "JetBrainsRuntime";
    rev = "jb${version}";
    hash = "sha256-7Nx7Y12oMfs4zeQMSfnUaDCW1xJYMEkcoTapSpmVCfU=";
  };

  meta = with lib; {
    description = "An OpenJDK fork to better support Jetbrains's products.";
    longDescription = ''
     JetBrains Runtime is a runtime environment for running IntelliJ Platform
     based products on Windows, Mac OS X, and Linux. JetBrains Runtime is
     based on OpenJDK project with some modifications. These modifications
     include: Subpixel Anti-Aliasing, enhanced font rendering on Linux, HiDPI
     support, ligatures, some fixes for native crashes not presented in
     official build, and other small enhancements.

     JetBrains Runtime is not a certified build of OpenJDK. Please, use at
     your own risk.
    '';
    homepage = "https://confluence.jetbrains.com/display/JBR/JetBrains+Runtime";
    inherit (openjdk17.meta) license platforms mainProgram;
    maintainers = with maintainers; [ edwtjo ];

    broken = stdenv.isDarwin;
  };

  passthru = oldAttrs.passthru // {
    home = "${jetbrains.jdk}/lib/openjdk";
  };
})
