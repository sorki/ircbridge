{ config, pkgs, lib, ... }:
{
  services.charybdis = {
    enable = true;
    config = builtins.readFile ./files/ircd.conf.example;
  };
}
