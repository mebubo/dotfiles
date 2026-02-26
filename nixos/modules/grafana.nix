{ ... }:

{
  services.grafana = {
    enable = true;
    settings = {
      security = {
        secret_key = "abc";
      };
    };
  };
}
