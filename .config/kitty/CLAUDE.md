- when adding keybindings, they should always use the tmux mode, e.g `map --mode tmux R load_config_file` unless stated
- when reading `man` pages, use `MANPAGER='cat' man <item to read>` e.g `MANPAGER='cat' man rg`. This ensures `man` is not interactive.
- use vim fold markers for grouping config sections:

```
#: section_name {{{

content here

#: }}}
```
