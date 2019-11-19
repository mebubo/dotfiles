{ configuration }:

# http://www.haskellforall.com/2018/08/nixos-in-production.html
import <nixpkgs/nixos> {
  system = "x86_64-linux";

  configuration = {
    imports = [
      configuration
    ];
  };
}
