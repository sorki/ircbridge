{ pkgs ? import <nixpkgs> {}
, compiler ? null
}:
let
  lib = pkgs.lib;
  overlay = import ./overlay.nix pkgs;
  overrideHaskellPackages = orig: {
    buildHaskellPackages =
      orig.buildHaskellPackages.override overrideHaskellPackages;
    overrides = if orig ? overrides
      then pkgs.lib.composeExtensions orig.overrides overlay
      else overlay;
  };

  packageSet =
    if compiler == null
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};
  haskellPackages = packageSet.override overrideHaskellPackages;
in {
  inherit (haskellPackages)
    ircbridge-ircbot-amqp
    ircbridge-ircbot-multi
    ircbridge-ircbot-zre;

  haskellPackages = lib.dontRecurseIntoAttrs haskellPackages;
  pkgs = lib.dontRecurseIntoAttrs pkgs;
}
