{ config, pkgs, lib, ... }:
let
  stateVersion = "23.11";

  ctsHostAddr = "172.17.0.1";
  ctsIrcHackintAddr = "172.17.0.2";
in
{
  imports = [
    # for ircbridge overlay only
    ./ircbridge.nix
  ];

  networking = {
    hostName = "ircbridge-example";
    nat = {
      enable = true;
      internalInterfaces = ["ve-ircbridge"];
      # FIXME: adjust according to your setup
      externalInterface = "ens1";
    };
  };

  # enable overlay on host so container can pick it up
  services.ircbridge.enableOverlay = true;

  containers.ircbridge = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = ctsHostAddr;
    localAddress = ctsIrcHackintAddr;
    # FIXME: disable if needed
    ephemeral = true;
    config = {
      imports = [
        ./amqp.nix
        ./ircbridge.nix
      ];

      # Use systemd-resolved inside the container
      networking.useHostResolvConf = lib.mkForce false;
      services.resolved.enable = true;

      services.stunnel = {
        enable = true;
        clients.irc-hackint = {
          accept = "127.0.0.1:6697";
          connect = "irc.hackint.org:6697";
        };
      };

      services.ircbridge = {
        enable = true;
        irc = {
          nick = "ircbridge";
          hostname = "ircbridge@example.org";
          host = "127.0.0.1";
          port = 6697;
          channels = [ "#bottest" ];
        };
      };

      system.stateVersion = stateVersion;
    };
  };

  system.stateVersion = stateVersion;
}
