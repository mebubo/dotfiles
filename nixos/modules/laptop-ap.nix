{ config, lib, pkgs, ... }:

{
  systemd.services.laptop-ap = {
    description = "Laptop WiFi Access Point";
    after = [ "network.target" ];
    wantedBy = []; # Disabled by default
    path = with pkgs; [ iw iproute2 iptables ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    script = ''
      iw phy phy0 interface add ap0 type __ap

      ip link set dev ap0 up
      ip addr add 192.168.5.1/24 dev ap0

      echo 1 > /proc/sys/net/ipv4/ip_forward

      iptables -F
      iptables -t nat -F
      iptables -P FORWARD DROP
      iptables -A FORWARD -i ap0 -o wlp1s0 -j ACCEPT
      iptables -A FORWARD -i wlp1s0 -o ap0 -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
      iptables -t nat -A POSTROUTING -o wlp1s0 -j MASQUERADE
    '';

    preStop = ''
      iptables -F
      iptables -t nat -F

      echo 0 > /proc/sys/net/ipv4/ip_forward

      ip addr del 192.168.5.1/24 dev ap0
      ip link set ap0 down

      iw dev ap0 del
    '';
  };

  # Hostapd service for the AP
  systemd.services.laptop-ap-hostapd = {
    description = "Hostapd for Laptop AP";
    requires = [ "laptop-ap.service" ];
    after = [ "laptop-ap.service" ];
    wantedBy = [];
    path = with pkgs; [ hostapd coreutils iw gnugrep gawk ];

    serviceConfig = {
      Type = "simple";
      RuntimeDirectory = "laptop-ap";
      ExecStart = "${pkgs.hostapd}/bin/hostapd /var/etc/laptop-ap/hostapd.conf";
    };

    preStart = ''
      set -eu pipefail

      # Define path for persistent config and runtime hostapd config
      LAPTOP_AP_CONFIG_DIR="/etc/laptop-ap"
      LAPTOP_AP_CHANNEL_FILE="$LAPTOP_AP_CONFIG_DIR/channel"
      HOSTAPD_RUNTIME_CONF="/var/etc/laptop-ap/hostapd.conf"
      DEFAULT_CHANNEL="36"
      UPSTREAM_IFACE="wlp1s0" # Define the upstream interface

      # --- Try to detect upstream channel ---
      DETECTED_CHANNEL=""
      DETECTED_CHANNEL=$(iw dev "$UPSTREAM_IFACE" info | grep -oP '^\s*channel \K\d+' || true)

      FINAL_CHANNEL=""
      if [ -n "$DETECTED_CHANNEL" ]; then
        echo "Detected channel $DETECTED_CHANNEL from $UPSTREAM_IFACE"
        FINAL_CHANNEL="$DETECTED_CHANNEL"
      else
        echo "Could not detect channel from $UPSTREAM_IFACE, falling back to config file/default."
        CONFIG_CHANNEL=$(cat "$LAPTOP_AP_CHANNEL_FILE" 2>/dev/null || echo "$DEFAULT_CHANNEL")
        if [ -z "$CONFIG_CHANNEL" ]; then
          FINAL_CHANNEL="$DEFAULT_CHANNEL"
          echo "Using default channel $DEFAULT_CHANNEL"
        else
          FINAL_CHANNEL="$CONFIG_CHANNEL"
          echo "Using channel $FINAL_CHANNEL from $LAPTOP_AP_CHANNEL_FILE"
        fi
      fi

      FINAL_HW_MODE=""
      if [ "$FINAL_CHANNEL" -le 14 ]; then
        FINAL_HW_MODE="g" # 2.4 GHz
        echo "Using hw_mode=g for channel $FINAL_CHANNEL"
      else
        FINAL_HW_MODE="a" # 5 GHz
        echo "Using hw_mode=a for channel $FINAL_CHANNEL"
      fi

      echo "Checking for existing hostapd config at $HOSTAPD_RUNTIME_CONF"
      if [ ! -f "$HOSTAPD_RUNTIME_CONF" ]; then
        echo "Generating hostapd config with channel $FINAL_CHANNEL and hw_mode $FINAL_HW_MODE"
        mkdir -p "$(dirname "$HOSTAPD_RUNTIME_CONF")"
        cat > "$HOSTAPD_RUNTIME_CONF" << EOF
      interface=ap0
      driver=nl80211
      ssid=name
      hw_mode=$FINAL_HW_MODE
      channel=$FINAL_CHANNEL
      ieee80211d=1
      ieee80211h=1
      ieee80211n=1
      vht_oper_chwidth=1
      wmm_enabled=0
      auth_algs=1
      wpa=2
      wpa_key_mgmt=WPA-PSK
      wpa_passphrase=passphrase
      wpa_pairwise=CCMP
      rsn_pairwise=CCMP
      country_code=FR
      EOF
      else
        echo "Using existing hostapd config at $HOSTAPD_RUNTIME_CONF"
      fi
    '';
  };

  systemd.services.laptop-ap-dnsmasq = {
    description = "DHCP Server for Laptop AP";
    requires = [ "laptop-ap.service" ];
    after = [ "laptop-ap.service" ];
    wantedBy = [];
    path = with pkgs; [ dnsmasq ];

    serviceConfig = {
      ExecStart = "${pkgs.dnsmasq}/bin/dnsmasq -C /var/etc/dnsmasq.ap0.conf --no-daemon";
    };

    preStart = ''
      mkdir -p /var/etc
      cat > /var/etc/dnsmasq.ap0.conf <<EOF
      interface=ap0
      dhcp-range=192.168.5.100,192.168.5.200,255.255.255.0,12h
      dhcp-leasefile=/tmp/dhcp.ap0.leases
      EOF
    '';
  };

  # Target to start everything together
  systemd.targets.laptop-ap = {
    description = "Laptop Access Point";
    requires = [
      "laptop-ap.service"
      "laptop-ap-hostapd.service"
      "laptop-ap-dnsmasq.service"
    ];
    wantedBy = [];
  };
}
