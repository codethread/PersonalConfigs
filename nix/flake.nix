{
  description = "codethread's system configuration — macOS (nix-darwin) + NixOS";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nix-darwin, home-manager, ... }:
  let
    # Each host picks a profile from nix/modules/profiles/ (or home.nix for full)
    hmFor = profile: {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.codethread = import profile;
    };
  in {
    # macOS — run: darwin-rebuild switch --flake .#macbook
    # Change "macbook" to match your machine hostname (scutil --get LocalHostName)
    darwinConfigurations.macbook = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin"; # Intel Mac: x86_64-darwin
      modules = [
        ./hosts/darwin
        home-manager.darwinModules.home-manager
        (hmFor ./modules/profiles/full.nix) # full package set
      ];
    };

    # NixOS — run: sudo nixos-rebuild switch --flake .#nixos
    # Rename "nixos" to match your machine hostname if preferred
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hosts/nixos
        home-manager.nixosModules.home-manager
        (hmFor ./modules/profiles/homelab.nix) # minimal: low-disk / VM
      ];
    };
  };
}
