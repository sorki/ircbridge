{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.ircbridge;
  hsOverlay =
    self: super: {
      haskellPackages = super.haskellPackages.override (old: {
        overrides = (import ../overlay.nix pkgs);
      });
    };

  bridgeExecutable = type: "${pkgs.haskellPackages."ircbridge-ircbot-${type}"}/bin/ircbridge-ircbot-${type}";
  bridgeExec = x: ''
    ${bridgeExecutable x.type} \
      --debug \
      --nick "${x.irc.nick}" \
      --username "${x.irc.username}" \
      --hostname "${x.irc.hostname}" \
      --realname "${x.irc.realname}" \
      --server "${x.irc.host}" \
      --port ${builtins.toString x.irc.port} \
      --burst-length ${builtins.toString x.irc.burstLength} \
      --delay-ms ${builtins.toString x.irc.delayMs} \
      ${lib.concatStringsSep " "  (map (x: ''"${x}"'') x.irc.channels)}
  '';
in
{
  options = {
    services.ircbridge = {
      enable = mkEnableOption "Enable ircbridge";
      type = mkOption {
        type = types.enum [ "amqp" "zre" "multi" ];
        default = "amqp";
      };

      irc = {
        host = mkOption {
          type = types.str;
          default = "localhost";
        };

        port = mkOption {
          type = types.port;
          default = 6667;
        };

        nick = mkOption {
          type = types.str;
          default = "ircbridge";
        };

        realname = mkOption {
          type = types.str;
          default = "ircbridge demo";
          description = "ircbridge IRC realname";
        };

        username = mkOption {
          type = types.str;
          default = "ircbridge";
          description = "ircbridge IRC user name";
        };

        hostname = mkOption {
          type = types.str;
          default = "ircbridge@example.org";
          description = "ircbridge hostname";
        };

        channels = mkOption {
          type = types.listOf types.str;
          default = [ "bottest" ];
        };

        burstLength = mkOption {
          type = types.int;
          default = 2;
          description = "Start rate-limiting after burst of this length was received.";
        };

        delayMs = mkOption {
          type = types.int;
          default = 1000000;
          description = "Delay between messages when rate-limited.";
        };
      };
    };
  };
  config =
    let
      enabled = cfg.enable;
      isAmqp = cfg.type == "amqp";
      isZre = cfg.type == "zre";
      isMulti = cfg.type == "multi";
    in
    mkMerge [
    (mkIf enabled {
      nixpkgs.overlays = [ hsOverlay ];
    })

    (mkIf (enabled && isAmqp && !isMulti) { # multi would be run via services.zre
      environment.systemPackages = with pkgs; [
        haskellPackages.amqp-utils
      ];

      systemd.services.ircbridge-amqp = {
        description = "AMQP ircbridge";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "rabbitmq.service" ];
        serviceConfig = {
          User = "ircbridge";
          Group = "ircbridge";
          ExecStart = bridgeExec cfg;
          Restart = "always";
          RestartSec = 1;
          MemoryMax = "100M";
          CPUQuota = "50%";
          DynamicUser="yes";
          PrivateTmp="yes";
          ProtectSystem="yes";
          ProtectHome="yes";
          ProtectDevices="yes";
          ProtectKernelTunables="yes";
          ProtectKernelModules="yes";
          ProtectControlGroups="yes";
          MemoryDenyWriteExecute="yes";
          PrivateMounts="yes";
        };
      };
    })

    (mkIf (enabled && (isZre || isMulti))  {
      environment.systemPackages = with pkgs; [
        haskellPackages.amqp-utils
        haskellPackages.zre
        haskellPackages.ircbridge-zre-util
      ];

      /*
      # Disabled for now as it requires zre overlay
      # and the module system is not happy
      # even when zre or multi are both disabled.
      # Should be lazy!
      services.zre.services = {
        name = "ircbridge-${cfg.type}";
        exe = bridgeExec cfg;
        after = lib.optional isMulti "rabbitmq.service";
        debug = true;
      };
      */
    })
  ];
}