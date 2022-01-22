self: super:

{
  chromium = super.chromium.override {
    commandLineArgs = [
      "--enable-features=VaapiVideoDecoder"
        "--enable-features=UseOzonePlatform" "--ozone-platform=wayland"
    ];
  };
}
