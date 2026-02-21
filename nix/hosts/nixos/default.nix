{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # --- Boot ---
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # --- Nix ---
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nixpkgs.config.allowUnfree = true;

  # --- Networking ---
  networking.hostName = "nixos"; # change to match your machine hostname
  networking.networkmanager.enable = true;

  # --- Locale ---
  time.timeZone = "Europe/London"; # adjust as needed
  i18n.defaultLocale = "en_GB.UTF-8";

  # --- Shell ---
  programs.nushell.enable = true; # adds nushell to /etc/shells

  # --- User ---
  users.users.codethread = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "podman" ];
    shell = pkgs.nushell;
  };

  # Swap: skipped in installer. Enable zram if you need swap pressure relief
  # (e.g. many browser tabs, build tools). Prefer this over a swap partition on
  # SSD — compresses RAM in-place, zero disk writes. For hibernation (suspend-to-disk)
  # you'd need a swap partition >= RAM size instead.
  # zramSwap.enable = true;

  # Minimal system packages — everything else is via home-manager
  environment.systemPackages = with pkgs; [
    git   # needed to clone dotfiles on fresh install
    curl
    wget
    make
  ];

  system.stateVersion = "24.11";
}
