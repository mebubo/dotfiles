{ ... }:

{
  networking.wireless = {
    enable = true;
    networks = {
      network1 = {
        psk = "psk1";
      };
    };
  };
}
