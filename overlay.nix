pkgs: hself: hsuper: {
  # bounds, till 0.6.6.2 is out https://github.com/stepcut/ircbot/pull/9
  ircbot =
    pkgs.haskell.lib.markUnbroken (pkgs.haskell.lib.doJailbreak hsuper.ircbot);
  ircbridge-types =
    hself.callCabal2nix "ircbridge-types"       ./ircbridge-types        {};
  ircbridge-aeson =
    hself.callCabal2nix "ircbridge-aeson"       ./ircbridge-aeson        {};
  ircbridge-amqp =
    hself.callCabal2nix "ircbridge-amqp"        ./ircbridge-amqp         {};
  ircbridge-amqp-util =
    hself.callCabal2nix "ircbridge-amqp-util"   ./ircbridge-amqp-util    {};
  ircbridge-cereal =
    hself.callCabal2nix "ircbridge-cereal"      ./ircbridge-cereal       {};
  ircbridge-ircbot =
    hself.callCabal2nix "ircbridge-ircbot"      ./ircbridge-ircbot       {};
  ircbridge-ircbot-amqp =
    hself.callCabal2nix "ircbridge-ircbot-amqp" ./ircbridge-ircbot-amqp  {};
  ircbridge-ircbot-multi =
    hself.callCabal2nix "ircbridge-ircbot-multi"./ircbridge-ircbot-multi {};
  ircbridge-ircbot-zre =
    hself.callCabal2nix "ircbridge-ircbot-zre"  ./ircbridge-ircbot-zre   {};
  ircbridge-optparse =
    hself.callCabal2nix "ircbridge-optparse"    ./ircbridge-optparse     {};
  ircbridge-pretty =
    hself.callCabal2nix "ircbridge-pretty"      ./ircbridge-pretty       {};
  ircbridge-zre =
    hself.callCabal2nix "ircbridge-zre"         ./ircbridge-zre          {};
  ircbridge-zre-util =
    hself.callCabal2nix "ircbridge-zre-util"    ./ircbridge-zre-util     {};
}
