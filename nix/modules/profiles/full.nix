{ pkgs, lib, ... }:

let
  isDarwin = pkgs.stdenv.isDarwin;
in {
  home.stateVersion = "24.11";

  home.packages = with pkgs; [

    # --- Languages ---
    go
    rustup
    just
    python311
    zig

    # --- Node.js ecosystem ---
    bun
    deno
    prettierd
    # volta: nix-managed node versions can conflict with volta's toolchain switching;
    # if you use volta, install it manually or via brew on macOS
    # volta

    # --- Shell ---
    atuin
    starship
    neovim
    tmux
    smug
    carapace

    # --- Utils ---
    git-lfs
    dust
    bat
    fd
    fzf
    ripgrep
    sd
    stylua
    wakatime-cli
    jq
    fx
    uv
    tokei
    tree
    grc
    ast-grep
    ffmpeg
    yt-dlp
    coreutils
    fswatch
    lazygit
    difftastic
    entr
    luarocks
    qmk

    # --- Work: AWS / infra ---
    awscli2
    buf
    glab
    grpcui
    grpcurl
    vault
    podman
    podman-compose
    miller

    # TODO: verify/add overlays for packages not yet in nixpkgs:
    #   cargo-lambda  — check pkgs.cargo-lambda
    #   vault-token-helper — third-party, may need overlay
    #   opencode — new tool, check current nixpkgs status

  ] ++ lib.optionals isDarwin [

    # macOS-only CLI tools
    terminal-notifier
    # pam-reattach — check pkgs.pam-reattach (may be darwin-only nixpkg)
    # ical-buddy — check pkgs.ical-buddy

  ] ++ lib.optionals (!isDarwin) [

    # Linux GUI apps — available in nixpkgs (not via Homebrew casks)
    kitty
    obsidian
    discord
    spotify
    _1password-gui
    ticktick # check: may not be in nixpkgs, fallback: install manually

  ];
}
