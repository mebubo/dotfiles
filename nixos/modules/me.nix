{ lib, ... }:

{

  options.me = {
    wifi-interface = lib.mkOption {
      type = lib.types.str;
    };
    private = lib.mkOption {
      type = lib.types.attrs;
      default = {};
    };
  };

}
