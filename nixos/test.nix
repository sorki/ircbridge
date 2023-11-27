import <nixpkgs/nixos/tests/make-test-python.nix> ({pkgs, ...}: rec {
  name = "ircbridge";
  meta = with pkgs.lib.maintainers; {
    maintainers = [ sorki ];
  };

  nodes.machine = {
    imports = [
      ./amqp.nix
      ./charybdis.nix
      ./ircbridge.nix
    ];

    services.ircbridge = {
      enable = true;
      irc.nick = "testbot";
    };

    services.getty.autologinUser = "root";
    environment.systemPackages = with pkgs; [
      irssi
    ];
  };

  testScript = ''
    machine.start()
    machine.wait_for_unit("charybdis.service")
    machine.wait_for_unit("rabbitmq.service")
    machine.wait_for_unit("ircbridge-amqp.service")
    machine.sleep(10) # give ircbot some time to connect
  '';
})
