{
  imports = [ ./common.nix ];

  # Only social apps that have no place on a work machine. Everything else —
  # editors, CLIs, defaults — comes from ./common.nix.
  homebrew.casks = [
    "whatsapp" # Native desktop client for WhatsApp
    "discord" # Voice and text chat software
  ];
}
