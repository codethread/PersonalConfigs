{ pkgs, lib, ... }:

# Minimal profile for a homelab/VM/low-disk machine.
# Heavy tools (ffmpeg, rust, python, zig full toolchain) omitted.
# Add to this as needed; graduate to modules/home.nix for full installs.

{
  home.stateVersion = "24.11";

  # Clone PersonalConfigs and run dotfile setup on first activation.
  # Subsequent activations are skipped (directory already exists).
  home.activation.bootDotfiles = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    DOTFILES="$HOME/PersonalConfigs"
    if [ ! -d "$DOTFILES" ]; then
      echo ">>> Cloning PersonalConfigs..."
      ${pkgs.git}/bin/git clone --branch linux \
        https://github.com/codethread/PersonalConfigs.git "$DOTFILES"
      echo ">>> Running boot machine..."
      export DOTFILES
      export XDG_CONFIG_HOME="$DOTFILES/config"
      export XDG_DATA_HOME="$HOME/.local/share"
      export XDG_STATE_HOME="$HOME/.local/state"
      export XDG_CACHE_HOME="$HOME/.local/cache"
      ${pkgs.nushell}/bin/nu \
        --env-config "$DOTFILES/config/nushell/env.nu" \
        --config "$DOTFILES/config/nushell/config.nu" \
        --commands "boot machine"
    fi
  '';

  # Override desktop entries for Electron apps to force --disable-gpu in VMs.
  # Without this the GPU process crashes silently and windows never appear.
  xdg.desktopEntries = {
    obsidian = {
      name = "Obsidian";
      exec = "${pkgs.obsidian}/bin/obsidian --disable-gpu %u";
      icon = "obsidian";
      categories = [ "Office" ];
      mimeType = [ "x-scheme-handler/obsidian" ];
    };
    google-chrome = {
      name = "Google Chrome";
      exec = "${pkgs.google-chrome}/bin/google-chrome-stable --disable-gpu %U";
      icon = "google-chrome";
      categories = [ "Network" "WebBrowser" ];
      mimeType = [ "text/html" "text/xml" "application/xhtml+xml" "x-scheme-handler/http" "x-scheme-handler/https" ];
    };
  };

  home.packages = with pkgs; [

    # --- Editors / Terminal ---
    neovim
    kitty
    google-chrome

    # --- Languages ---
    go
    zig
    bun    # runtime for oven/ CLI tools

    # --- Shell essentials ---
    atuin
    starship
    tmux
    smug
    carapace
    fzf
    bat
    fd
    ripgrep
    jq
    sd
    tree
    dust

    # --- Git ---
    git-lfs
    lazygit
    difftastic

    # --- Apps ---
    # spotify: x86_64 Linux only, no ARM binary — excluded from aarch64 VM
    obsidian

    # --- Utils ---
    just
    uv    # python env manager without the full python install
    fx
    tokei

  ];
}
