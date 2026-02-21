{ pkgs, ... }:

{
  # --- Hyprland (Wayland compositor) ---
  programs.hyprland.enable = true;

  # Display manager — tuigreet runs as system 'greeter' user (auto-created by greetd)
  services.greetd = {
    enable = true;
    settings.default_session = {
      command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd Hyprland";
      user = "greeter";
    };
  };

  # --- Fonts (matches kitty.conf: Victor Mono + Nerd Symbols) ---
  fonts.packages = with pkgs; [
    victor-mono
    nerd-fonts.symbols-only
  ];

  # --- Desktop packages ---
  environment.systemPackages = with pkgs; [
    kitty           # terminal

    # Launcher (spotlight-like)
    rofi

    # Status bar
    waybar

    # Wallpaper (solid rose pine background)
    swaybg

    # Notifications
    mako

    # Theme
    rose-pine-gtk-theme
    rose-pine-icon-theme

    # Wayland utils
    wl-clipboard
    grim            # screenshot
    slurp           # screen area selector
  ];

  # Propagate GTK theme into sessions
  environment.sessionVariables = {
    GTK_THEME = "rose-pine";
  };
}
