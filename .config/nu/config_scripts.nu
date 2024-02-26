use ct/core *

source ct/alias/mod.nu

$env.config = {
  show_banner: false,
  keybindings: [], # needed for atuin to not blow up
  hooks: {},
  completions: {}
  menus: []
  use_ansi_coloring: false
  history: {
      max_size: 100_000 # Session has to be reloaded for this to take effect
      sync_on_enter: false
      file_format: "plaintext" # hide scripting history from usual REPL stuff
  }
}
