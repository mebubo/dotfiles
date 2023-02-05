# based on https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/jetbrains/linux.nix

{ stdenv, lib, makeWrapper, patchelf, writeText
, coreutils, gnugrep, which, git, unzip, libsecret, libnotify, e2fsprogs
, zlib, jdk, maven, fetchurl, openjdk }:

let

  plugin-scala = fetchurl {
    name = "intellij-plugin-scala.zip";
    url = "https://plugins.jetbrains.com/plugin/download?rel=true&updateId=279698";
    sha256 = "sha256-s8O0tcXvAluLUyDnJ+wkacqzUsMY7yA5Iib/qmVvlsA=";
  };

  plugin-vim = fetchurl {
    name = "intellij-plugin-vim.zip";
    url = "https://plugins.jetbrains.com/plugin/download?rel=true&updateId=275091";
    sha256 = "sha256-M0mifWorVGcLaZhbP4jJH2SsTztziYn5SgCJlDderxA=";
  };

  version = "2022.3.2";

  intellij = fetchurl {
    url = "https://download.jetbrains.com/idea/ideaIC-${version}-aarch64.tar.gz";
    sha256 = "sha256-cOsTwMoOl/nIV1/2srdYr+wkn1itfd8xSbbjEK6h4H4=";
  };

  keep = [
    "java"
    "java-ide-customization"
    "git4idea"
    "junit"
  ];

  productShort = "IDEA";
  loName = lib.toLower productShort;
  hiName = lib.toUpper productShort;
  vmoptsName = loName
             + lib.optionalString stdenv.hostPlatform.is64bit "64"
             + ".vmoptions";
  vmopts = null;

in

stdenv.mkDerivation (rec {

  inherit version;

  pname = "idea-community";

  src = intellij;

  vmoptsFile = lib.optionalString (vmopts != null) (writeText vmoptsName vmopts);

  nativeBuildInputs = [ makeWrapper patchelf unzip ];

  postPatch = ''
      get_file_size() {
        local fname="$1"
        echo $(ls -l $fname | cut -d ' ' -f5)
      }

      munge_size_hack() {
        local fname="$1"
        local size="$2"
        strip $fname
        truncate --size=$size $fname
      }

      rm -rf jbr

      interpreter=$(echo ${stdenv.cc.libc}/lib/ld-linux*.so.2)

      # target_size=$(get_file_size bin/fsnotifier)
      patchelf --set-interpreter "$interpreter" bin/fsnotifier
      patchelf bin/libdbm.so
      # munge_size_hack bin/fsnotifier $target_size
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/{bin,$pname,share/pixmaps,libexec/${pname}}
    cp -a . $out/$pname
    [[ -f $out/$pname/bin/${loName}.png ]] && ln -s $out/$pname/bin/${loName}.png $out/share/pixmaps/${pname}.png
    [[ -f $out/$pname/bin/${loName}.svg ]] && ln -s $out/$pname/bin/${loName}.svg $out/share/pixmaps/${pname}.svg
    mv bin/fsnotifier* $out/libexec/${pname}/.

    jdk=${jdk.home}

    makeWrapper "$out/$pname/bin/${loName}.sh" "$out/bin/${pname}" \
      --prefix PATH : "$out/libexec/${pname}:${lib.makeBinPath [ jdk coreutils gnugrep which git ]}" \
      --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [
        # Some internals want libstdc++.so.6
        stdenv.cc.cc.lib libsecret e2fsprogs libnotify zlib
      ]}" \
      --set-default JDK_HOME "$openjdk" \
      --set-default ANDROID_JAVA_HOME "$openjdk" \
      --set-default JAVA_HOME "$openjdk" \
      --set-default JETBRAINSCLIENT_JDK "$jdk" \
      --set M2_HOME "${maven}/maven" \
      --set M2 "${maven}/maven/bin" \
      --set ${hiName}_JDK "$jdk" \
      --set ${hiName}_VM_OPTIONS ${vmoptsFile}

    runHook postInstall
  '';

  postFixup = ''
    (
      cd "$(find $out -name plugins)"

      for p in *; do
        case $p in
          ${builtins.concatStringsSep "|" keep})
            ;;
          *)
            rm -fr $p
            ;;
        esac
      done

      ${unzip}/bin/unzip ${plugin-scala}
      ${unzip}/bin/unzip ${plugin-vim}
    )
  '';

  preferLocalBuild = true;

})
