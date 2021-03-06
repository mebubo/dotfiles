{ ... }:

{
  services.prometheus = {
    enable = true;
    listenAddress = "localhost";
    exporters = {
      node = {
        enable = true;
        disabledCollectors = [ "rapl" ];
        listenAddress = "localhost";
      };
    };
    scrapeConfigs = [
      {
        job_name = "node";
        scrape_interval = "10s";
        static_configs = [
          {
            targets = [ "localhost:9100" ];
          }
        ];
      }
    ];
  };
}
