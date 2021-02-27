self: super:

{
  st = super.st.override {
    conf = builtins.readFile ./config.h;
  };
}
