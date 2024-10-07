{
    # NOTE: helpful links
    # - https://nix.dev/manual/nix/2.18/language/
    # - https://github.com/NixOS/nixpkgs

    description = "Example Darwin system flake";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
        nix-darwin.url = "github:LnL7/nix-darwin";
        nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

        # TODO: figure out how to do a binary install if possible
        # also https://github.com/nix-community/kickstart-nix.nvim might help
        neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    };

    outputs = inputs@{ self, nix-darwin, nixpkgs, neovim-nightly-overlay }:
        let
            configuration = { pkgs, ... }: {

                # List packages installed in system profile. To search by name, run:
                # $ nix-env -qaP | grep wget
                environment.systemPackages =
                    [ 
                        pkgs.vim
                        neovim-nightly-overlay.packages.${pkgs.system}.default
                        pkgs.nushell
                        pkgs.tmux
                        pkgs.fd
                        pkgs.fzf
                        pkgs.ripgrep
                        pkgs.starship
                        pkgs.coreutils #mac
                    ];

                environment.pathsToLink = [ "/Applications" ];

                environment.variables = {
                    EDITOR = "nvim";
                    # no .lesshst
                    LESSHISTFILE = "-"; 
                    XDG_CONFIG_HOME = "$HOME/.config";
                };

                environment.systemPath = [
                    "$HOME/.local/bin"
                ];

                # these will only work for zsh
                environment.shellAliases = {
                    # nix edit
                    ne = "nvim ~/PersonalConfigs/.config/nix-darwin/flake.nix";
                    # reload
                    nn = "git add ~/PersonalConfigs/.config/nix-darwin; git commit -m 'nix: update'; darwin-rebuild switch --flake ~/PersonalConfigs/.config/nix-darwin";
                    # nix help
                    nh = "man configuration.nix";
                    # open help in gui (useful for reference)
                    nhh = "darwin-help";
                    pathis = "echo $PATH | tr -s ':' '\n'";
                };


                # Auto upgrade nix package and the daemon service.
                services.nix-daemon.enable = true;

                # Necessary for using flakes on this system.
                nix.settings.experimental-features = "nix-command flakes";

                programs = {
                    # Create /etc/zshrc that loads the nix-darwin environment.
                    zsh = {
                        # NOTE: for now keeping with zsh over nushell, as nix
                        # does quite a bit of setup for things like paths and
                        # config files. Beyond my powers at this point. If this
                        # is removed, the zsh* files get removed entirely and
                        # then nix isn't in the path at all
                        enable = true; # don't remove this on mac
                    };
                };

                # Set Git commit hash for darwin-version.
                system.configurationRevision = self.rev or self.dirtyRev or null;

                # Used for backwards compatibility, please read the changelog before changing.
                # $ darwin-rebuild changelog
                system.stateVersion = 5;

                # The platform the configuration will be used on.
                nixpkgs.hostPlatform = "aarch64-darwin";

                security.pam.enableSudoTouchIdAuth = true;

                system.keyboard.enableKeyMapping = true;
                system.keyboard.remapCapsLockToEscape = true;

                # fonts.fontDir.enable = true; # will remove other fonts!, only on linux
                # https://github.com/NixOS/nixpkgs/blob/master/pkgs/data/fonts/nerdfonts/shas.nix
                fonts.packages = [ (pkgs.nerdfonts.override { fonts = [ "CascadiaCode" ]; }) ];

                # allow sudo touch in tmux too
                environment.etc."pam.d/sudo_local" = {
                    text = ''
                        auth       optional       ${pkgs.pam-reattach}/lib/pam/pam_reattach.so
                        auth       sufficient     pam_tid.so
                    '';
                };

                system.defaults = {
                    # not sure if this works
                    NSGlobalDomain.AppleEnableMouseSwipeNavigateWithScrolls = false;
                    NSGlobalDomain.AppleShowAllExtensions = true;
                    # time in ms till next key, lower feels faster
                    NSGlobalDomain.InitialKeyRepeat = 12;
                    NSGlobalDomain.KeyRepeat = 2;
                    NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
                    NSGlobalDomain.PMPrintingExpandedStateForPrint = true;
                    NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
                    NSGlobalDomain."com.apple.trackpad.trackpadCornerClickBehavior" = 1;

                    dock.autohide = true;
                    dock.mru-spaces = false;
                    dock.show-recents = false;
                    dock.static-only = true;
                    dock.expose-animation-duration = 0.01;

                    finder.AppleShowAllExtensions = true;
                    finder.FXEnableExtensionChangeWarning = false;

                    loginwindow.GuestEnabled = false;

                    # TODO:
                    # - login items
                    # - finder settings
                    # - shortcuts and various things to disable
                    # - remove widgets?
                    # - dock settings
                    # - anything fun?
                };

                homebrew = {
                    enable = true;
                    caskArgs.no_quarantine = true;
                    global.brewfile = true;
                    onActivation.cleanup = "uninstall";

                    # masApps = { magnet };
                    casks = [ 
                        "google-chrome" 
                        "whatsapp" 
                        "kitty" 
                        "alfred" 
                    ];
                    # brews = [ "google-chrome"];
                };
            };
        in
            {
            # Build darwin flake using:
            # $ darwin-rebuild build --flake .#simple
            darwinConfigurations."codethreads-MacBook-Air" = nix-darwin.lib.darwinSystem {
                modules = [ configuration ];
            };

            # Expose the package set, including overlays, for convenience.
            darwinPackages = self.darwinConfigurations."codethreads-MacBook-Air".pkgs;
        };
}
