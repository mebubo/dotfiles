{ pkgs, lib, ... }:

{

  hardware.opengl.enable = true;

  programs.sway = {
    enable = true;
    extraPackages = lib.mkForce (with pkgs; [ xwayland dmenu alacritty ]);
  };

  users.users.me = {
    packages = with pkgs; [
        alacritty
        alsaUtils
        # chromium
        dmenu
        # firefox
        # mpv
        # pavucontrol
        # tigervnc
        xfce.xfce4-terminal
        youtube-dl
      ];
    };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  nixpkgs.overlays = [
    (self: super: {
      chromium = super.chromium.override {
        enableVaapi = true;
      };
    })
  ];

  swapDevices = [ { device = "/swapfile"; size = 1024; } ];

}
