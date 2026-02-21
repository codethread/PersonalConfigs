{ pkgs, ... }:

{
  # --- Nix daemon ---
  services.nix-daemon.enable = true;
  nix.settings.experimental-features = "nix-command flakes";

  nixpkgs.config.allowUnfree = true;

  # --- Shell ---
  # Register nushell as a valid login shell
  environment.shells = [ pkgs.nushell ];
  users.users.codethread.shell = pkgs.nushell;

  # --- Homebrew ---
  # nix-darwin manages Homebrew for casks and App Store apps not in nixpkgs.
  # Homebrew must be pre-installed: https://brew.sh
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = false;
      upgrade = false;
      cleanup = "zap"; # remove packages not listed here
    };
    taps = [
      "nikitabobko/tap" # aerospace
    ];
    casks = [
      "aerospace"
      "obsidian"
      "1password"
      "1password-cli"
      "alfred"
      "google-chrome"
      "kitty"
      "visual-studio-code"
      "cursor"
      "spotify"
      "zed"
      "ticktick"
      "font-fira-code"
      "font-victor-mono"
      "font-symbols-only-nerd-font"
    ];
    masApps = {
      "DaisyDisk" = 411643860;
      "Magnet"    = 441258766;
      "Spokenly"  = 6740315592;
    };
  };

  system.stateVersion = 5;
}
