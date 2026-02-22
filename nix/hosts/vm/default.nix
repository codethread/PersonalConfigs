{ ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos-system.nix
  ];

  networking.hostName = "vm";
}
