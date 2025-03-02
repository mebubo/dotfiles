{ pkgs, ... }:

let

  lima-create-default = pkgs.callPackage ./lima.nix {};

in

{

  home.packages = with pkgs; [
    lima
    lima-create-default
  ];

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "lima" = {
        hostname = "127.0.0.1";
        user = "me";
        port = 60022;
        extraOptions = {
          IdentityFile = "~/.lima/_config/user";
          NoHostAuthenticationForLocalhost = "yes";
          PreferredAuthentications = "publickey";
          Compression = "no";
          IdentitiesOnly = "yes";
          Ciphers = "^aes128-gcm@openssh.com,aes256-gcm@openssh.com";
        };
      };
    };
  };

}
