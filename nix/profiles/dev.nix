{ pkgs, ... }:

# Dev profile: the shared user environment plus hardware-specific extras.
#
# Only things tied to *this* machine's hardware belong here. Anything a laptop
# could plausibly want goes in features/common.nix instead — a few MB of unused
# packages is cheaper than two environments drifting apart.
# Used by: darwinConfigurations.dev

{
  imports = [
    ../features/common.nix
  ];

  home.packages = with pkgs; [
    qmk # keyboard flashing — the keyboards live on this desk
    dos2unix # qmk dep
  ];
}
