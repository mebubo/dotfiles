{ config, lib, pkgs, ... }:

with lib;

let

  xorg = pkgs.xorg;
  modules = [ xorg.xorgserver xorg.xf86inputlibinput ];
  fontsForXServer = config.fonts.fonts;

  configFile = (pkgs.runCommand "xserver.conf" {}
      ''
        echo 'Section "Files"' >> $out

        for i in ${toString fontsForXServer}; do
          if test "''${i:0:''${#NIX_STORE}}" == "$NIX_STORE"; then
            for j in $(find $i -name fonts.dir); do
              echo "  FontPath \"$(dirname $j)\"" >> $out
            done
          fi
        done

        for i in $(find ${toString modules} -type d); do
          if test $(echo $i/*.so* | wc -w) -ne 0; then
            echo "  ModulePath \"$i\"" >> $out
          fi
        done

        echo 'EndSection' >> $out

      '');

in

{

  config = {

    hardware.opengl.enable = true;

    environment.etc =
      {
        "xorg.conf" = {
          source = "${configFile}";
          target = "X11/xorg.conf";
        };
        "libinput.conf" = let
          cfgPath = "/X11/xorg.conf.d/40-libinput.conf";
        in
          {
            source = xorg.xf86inputlibinput.out + "/share" + cfgPath;
            target = cfgPath;
          };
      };

    environment.systemPackages =
      [ pkgs.xdg_utils
        xorg.setxkbmap
        xorg.xauth
        xorg.xinit
        xorg.xinput
        xorg.xprop
        xorg.xrandr
        xorg.xset
      ];

    environment.pathsToLink = [ "/etc/xdg" "/share/xdg" "/share/applications" "/share/icons" "/share/pixmaps" ];

    fonts.enableDefaultFonts = mkDefault true;

  };

}
