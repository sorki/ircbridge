attrs@{...}:
let
  inherit (import ./. attrs) pkgs haskellPackages;
  hslib = pkgs.haskell.lib;

  packages = [
    "ircbridge-types"
    "ircbridge-aeson"
    "ircbridge-amqp"
    "ircbridge-amqp-util"
    "ircbridge-cereal"
    "ircbridge-ircbot"
    "ircbridge-ircbot-amqp"
    "ircbridge-ircbot-multi"
    "ircbridge-ircbot-zre"
    "ircbridge-optparse"
    "ircbridge-pretty"
    "ircbridge-zre"
    "ircbridge-zre-util"
  ];

  extract-external-inputs = p:
    builtins.filter (dep: !(builtins.elem dep packages))
      (map (x: x.pname) (hslib.getHaskellBuildInputs haskellPackages.${p}));
  external-inputs = map (x: haskellPackages.${x}) (builtins.concatLists
    (map extract-external-inputs packages));
  package-envs = builtins.listToAttrs (map (p: {
    name = p;
    value = haskellPackages.${p}.env;
  }) packages);
in (haskellPackages.mkDerivation {
  pname = "ircbridge";
  version = "0.0.0.0";
  libraryHaskellDepends = external-inputs;
  license = pkgs.stdenv.lib.licenses.asl20;
}).env // package-envs
