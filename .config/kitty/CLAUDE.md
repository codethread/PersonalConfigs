- when adding keybindings, they should always use the tmux mode, e.g `map --mode tmux R combine : load_config_file : show_error Configuration reloaded!` unless stated
- when reading `man` pages, use `MANPAGER='cat' man <item to read>` e.g `MANPAGER='cat' man rg`. This ensures `man` is not interactive.
- use `pnpm` for package management instead of `npm`
- use vim fold markers for grouping config sections:

```
#: section_name {{{

content here

#: }}}
```
