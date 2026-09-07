{ ... }:

# Personal laptop profile: identical to dev, minus the machine-specific extras.
# Everything worth having lives in features/common.nix — see profiles/dev.nix
# for the rationale on what is allowed to diverge.
# Used by: darwinConfigurations.personal

{
  imports = [
    ../features/common.nix
  ];
}
