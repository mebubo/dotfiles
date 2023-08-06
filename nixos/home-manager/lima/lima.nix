{ pkgs, lima }:

pkgs.writeShellScriptBin "lima-create-default" ''
    ${lima}/bin/limactl start --name=default ${./lima.yaml}
''
