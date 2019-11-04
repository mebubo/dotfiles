self: super:

{
  polybar = super.polybar.override {
    pulseSupport = true;
  };
}
