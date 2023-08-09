{ pkgs, lima }:

pkgs.writeShellScriptBin "lima-create-default" ''
    ${lima}/bin/limactl create --name=default ${./lima.yaml}
''
