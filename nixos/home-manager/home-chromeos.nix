{ config, pkgs, lib, ... }:

let

  chromeos-scale = pkgs.writeShellScriptBin "chromeos-scale" ''
    sommelier -X --dpi=160 --scale=1.3 "$@";
  '';

  chromeOSWrappers = {
    inherit chromeos-scale;
    idea-community-chromeos = pkgs.writeShellScriptBin "idea-community-chromeos" ''${chromeos-scale}/bin/chromeos-scale ${pkgs.jetbrains.idea-community}/bin/idea-community "$@"'';
    code-chromeos = pkgs.writeShellScriptBin "code-chromeos" ''${chromeos-scale}/bin/chromeos-scale ${pkgs.vscode}/bin/code -w "$@"'';
  };

in

{

  home.packages = lib.attrValues chromeOSWrappers;

}
