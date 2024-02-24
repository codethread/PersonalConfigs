use ct/core *

source ct/alias/mod.nu

# use ct/dotty

$env.config = {
  show_banner: false,
  keybindings: [], # needed for atuin to not blow up
  hooks: {},
  completions: {}
  menus: []
  use_ansi_coloring: false
  history: {
      max_size: 100_000 # Session has to be reloaded for this to take effect
      sync_on_enter: true # Enable to share history between multiple sessions, else you have to close the session to write history to file
      file_format: "sqlite" # "sqlite" or "plaintext"
      # only available with sqlite file_format. true enables history isolation, false disables it. 
      # true will allow the history to be isolated to the current session using up/down arrows.
      # false will allow the history to be shared across all sessions.
      # disabled as this is purely per session... would be better with a custom function
      isolation: false 
  }
}
