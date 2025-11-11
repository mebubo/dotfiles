{ config, ... }:

{

  users.users.root = {
    openssh.authorizedKeys.keyFiles = config.me.private.keys;
  };

  users.users.me = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "dialout" ];
    openssh.authorizedKeys.keyFiles = config.me.private.keys;
  };

  services.openssh = {
    enable = true;
    settings = {
      KbdInteractiveAuthentication = false;
      PasswordAuthentication = false;
    };
  };

}
