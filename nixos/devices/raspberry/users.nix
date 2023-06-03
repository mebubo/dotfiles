{ ... }:

{

  users.users.root = {
    openssh.authorizedKeys.keyFiles = [ ./key.pub ./key2.pub ];
  };

  users.users.me = {
    isNormalUser = true;
    uid = 1000;
    openssh.authorizedKeys.keyFiles = [ ./key.pub ./key2.pub ];
  };

  services.openssh = {
    enable = true;
    settings = {
      KbdInteractiveAuthentication = false;
      PasswordAuthentication = false;
    };
  };

}
