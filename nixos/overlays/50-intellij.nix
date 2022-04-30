self: super:

let

  plugin-scala = self.fetchurl {
    name = "intellij-plugin-scala.zip";
    url = "https://plugins.jetbrains.com/plugin/download?rel=true&updateId=167520";
    sha256 = "19rh6crgq9p051qqvwwh59ygf8503545ysw52fylhps8r63zr33a";
  };

  plugin-vim = self.fetchurl {
    name = "intellij-plugin-vim.zip";
    url = "https://plugins.jetbrains.com/plugin/download?rel=true&updateId=169987";
    sha256 = "0xi56crlyl3ampygxhg3127cgmpgh9qpjw13sgvc6gxdh8890fxx";
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
