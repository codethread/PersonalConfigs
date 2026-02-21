# Machine-specific hardware configuration — NOT committed to the repo.
# Generate this on the target machine after NixOS install:
#
#   nixos-generate-config --show-hardware-config > nix/hosts/nixos/hardware-configuration.nix
#
# This file contains disk UUIDs and detected hardware specific to your machine.
# See MIGRATION.md for full NixOS setup instructions.
{ ... }: { }
