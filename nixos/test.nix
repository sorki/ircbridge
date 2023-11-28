import <nixpkgs/nixos/tests/make-test-python.nix> ({pkgs, ...}: rec {
  name = "ircbridge";
  meta = with pkgs.lib.maintainers; {
    maintainers = [ sorki ];
  };

  nodes.machine = {pkgs, ...}: {
    imports = [
      ./amqp.nix
      ./ircbridge.nix
    ];

    containers.irc = let localIP = "192.168.0.42"; in {
      autoStart = true;
      privateNetwork = true;
      hostAddress = "192.168.0.1";
      localAddress = localIP;
      config = {
        imports = [
          ./amqp.nix
          ./charybdis.nix
          ./ircbridge.nix
        ];

        services.ircbridge = {
          enable = true;
          irc = {
            nick = "ctbot";
            host = "localhost";
            port = 6666;
            channels = [ "#bottest" ];
          };
          irccat = {
            enable = true;
            defaultTarget = "#bottest";
            host = "0.0.0.0";
            port = 12347;
          };
        };

        systemd.services.test-spammer = {
          wantedBy = [ "multi-user.target" ];
          after = [ "ircbridge-amqp.service" ];
          script = ''
            ircbridge-amqp-cat -c '#bottest' yolo
          '';
          path = [ pkgs.haskellPackages.ircbridge-amqp-util ];
          serviceConfig = {
            Restart = "always";
            RestartSec = "3s";
          };
        };

        networking.firewall.allowedTCPPorts = [ 6666 12347 ];
        system.stateVersion = "666";
      };
    };

    services.ircbridge = {
      enable = true;
      irc = {
        nick = "testbot";
        host = "192.168.0.42";
        port = 6666;
        channels = [ "#bottest" ];
      };
      irccat = {
        enable = true;
        defaultTarget = "#bottest";
        sendAsNotice = true;
      };
    };

    systemd.services.test-tail = {
      wantedBy = [ "multi-user.target" ];
      after = [ "ircbridge-amqp.service" ];
      script = ''
        ircbridge-amqp-tail --simple
      '';
      path = [ pkgs.haskellPackages.ircbridge-amqp-util ];
    };

    services.journald.extraConfig = ''
      SyncIntervalSec=1s
    '';
  };

  testScript = ''
    machine.start()
    machine.wait_for_unit("container@irc.service")

    machine.wait_for_unit("rabbitmq.service")
    machine.wait_for_unit("ircbridge-amqp.service")
    machine.wait_for_unit("ircbridge-amqp-irccat-tcpserver.service")
    machine.wait_for_unit("test-tail.service")

    machine.sleep(60) # give bots some time to connect
    # send something via ctbot
    print(machine.succeed("systemd-run --pty --machine=irc -- /run/current-system/sw/bin/ircbridge-amqp-cat -c '#bottest' yay"))

    # send something via local irccat
    print(machine.succeed('echo "hello local irccat" | ${pkgs.netcat}/bin/nc -v localhost 12345'))

    # send something via remote irccat
    print(machine.succeed('echo "hello remote irccat" | ${pkgs.netcat}/bin/nc -v 192.168.0.42 12347'))

    machine.sleep(23)
    machine.succeed("journalctl -n 1000 -u test-tail")
    machine.succeed("journalctl -n 1000 -u test-tail | grep yay")
    machine.succeed("journalctl -n 1000 -u test-tail | grep yolo")
    machine.succeed("journalctl -n 1000 -u test-tail | grep 'remote irccat'")
  '';
})
