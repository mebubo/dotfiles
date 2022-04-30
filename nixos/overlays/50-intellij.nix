self: super:

let

  plugin-scala = self.fetchurl {
    name = "intellij-plugin-scala.zip";
    url = "https://plugins.jetbrains.com/plugin/download?rel=true&updateId=153523";
    sha256 = "0zbj4cvrpbam1mvl7kr8x943ixvlmvqd8k79i6rpyhf41cn2ngx5";
  };

  plugin-vim = self.fetchurl {
    name = "intellij-plugin-vim.zip";
    url = "https://plugins.jetbrains.com/plugin/download?rel=true&updateId=151325";
    sha256 = "1l814yfw1q1cr2qp9gnxhkj818sqvz5ncx8gnpx867d1mga451d2";
  };

  keep = [
    "java"
    "java-ide-customization"
    "git4idea"
    "junit"
  ];

in

{

  jetbrains = super.jetbrains // {
    idea-community-minimal = super.jetbrains.idea-community.overrideAttrs (attrs: {
      postFixup = ''
        (
          # cd $out/idea-*/plugins/
          # cd $out/Application*/Intelli*/Content*/plugins/
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

          ${self.unzip}/bin/unzip ${plugin-scala}
          ${self.unzip}/bin/unzip ${plugin-vim}
        )
      '';
    });
  };
}
