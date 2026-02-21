{ pkgs, lib, ... }:

# Minimal profile for a homelab/VM/low-disk machine.
# Heavy tools (ffmpeg, rust, python, zig full toolchain) omitted.
# Add to this as needed; graduate to modules/home.nix for full installs.

{
  home.stateVersion = "24.11";

  home.packages = with pkgs; [

    # --- Editors ---
    neovim

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
    spotify
    obsidian

    # --- Utils ---
    just
    uv    # python env manager without the full python install
    fx
    tokei

  ];
}
