use ct/core *

source ct/alias/mod.nu

$env.config = {
  show_banner: false,
  keybindings: [], # needed for atuin to not blow up
  hooks: {},
  completions: {}
  menus: []
  use_ansi_coloring: false
}
