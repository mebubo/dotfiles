{ ... }:

{

  users.users.root = {
    openssh.authorizedKeys.keyFiles = [ ./key.pub ];
  };

  users.users.me = {
    isNormalUser = true;
    uid = 1000;
    openssh.authorizedKeys.keyFiles = [ ./key.pub ];
  };

  services.openssh = {
    enable = true;
    kbdInteractiveAuthentication = false;
    passwordAuthentication = false;
  };

}
