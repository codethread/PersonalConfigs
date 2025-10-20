## Kitty Configuration

This directory contains configuration for the Kitty terminal emulator.

**Context:** When working in this directory, use `/teach-kitty` to load comprehensive Kitty documentation if not already shared.

**Structure:**

- `kitty.conf` - Main configuration file
- `keys.conf` - Keyboard bindings
- `geninclude_config.py` - Dynamic config generator (called via geninclude directive)
- `tab_bar.py` - Custom tab bar rendering
- `bin/` - Helper scripts and utilities
- `sessions/` - Session files for startup layouts
- `themes/` - Color schemes

## Config Style

- when adding keybindings, they should always use the tmux mode, e.g `map --mode tmux R load_config_file` unless stated
- use vim fold markers for grouping config sections:

  ```
  #: section_name {{{

  content here

  #: }}}
  ```

## Python code style

When writing Python scripts for Kitty (e.g., watchers, geninclude scripts):

- Follow Clean Code principles - structure like a newspaper with public API first
- Separate public and private sections with comments:

  ```python
  # Public API
  def generate_config(): ...

  # Private implementation
  def _helper_function(): ...
  ```

- Type hints for function signatures
- Keep public functions minimal and testable
