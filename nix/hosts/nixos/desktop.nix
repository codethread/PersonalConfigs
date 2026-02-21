{ pkgs, ... }:

{
  # --- Hyprland (Wayland compositor) ---
  programs.hyprland.enable = true;

  # Minimal display manager — launches Hyprland directly
  services.greetd = {
    enable = true;
    settings.default_session = {
      command = "${pkgs.hyprland}/bin/Hyprland";
      user = "codethread";
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
    rofi-wayland

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
