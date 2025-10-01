- when adding keybindings, they should always use the tmux mode, e.g `map --mode tmux R load_config_file` unless stated
- use vim fold markers for grouping config sections:

  ```
  #: section_name {{{

  content here

  #: }}}
  ```
