{
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
                    ];

                environment.variables = {
                    EDITOR = "nvim";
                    # no .lesshst
                    LESSHISTFILE = "-"; 

                };

                environment.systemPath = [
                    # "/Users/codethread/.local/bin"
                ];

                environment.shells = [ pkgs.nushell ];
                # these will only work for zsh
                environment.shellAliases = {
                    # nix edit
                    ne = "nvim ~/PersonalConfigs/.config/nix-darwin/flake.nix";
                    # reload
                    nn = "darwin-rebuild switch --flake ~/PersonalConfigs/.config/nix-darwin";
                    # nix help
                    nh = "man configuration.nix";
                    # open help in gui (useful for reference)
                    nhh = "darwin-help";
                };


                # Auto upgrade nix package and the daemon service.
                services.nix-daemon.enable = true;
                # nix.package = pkgs.nix;

                # Necessary for using flakes on this system.
                nix.settings.experimental-features = "nix-command flakes";

                # Create /etc/zshrc that loads the nix-darwin environment.
                programs = {
                    # zsh = {
                    #     enable = true;
                    # };
                };

                users.users = {
                    codethread = {
                        shell = pkgs.nushell;
                    };
                };

                system.activationScripts."nu-shell".text = "chsh -s ${pkgs.nushell}";

                # Set Git commit hash for darwin-version.
                system.configurationRevision = self.rev or self.dirtyRev or null;

                # Used for backwards compatibility, please read the changelog before changing.
                # $ darwin-rebuild changelog
                system.stateVersion = 5;

                # The platform the configuration will be used on.
                nixpkgs.hostPlatform = "aarch64-darwin";

                security.pam.enableSudoTouchIdAuth = true;
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
