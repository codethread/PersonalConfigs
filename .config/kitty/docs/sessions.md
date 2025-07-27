# Kitty Session Management

A tmux-like session system for kitty that creates separate OS windows for different development contexts.

## Overview

This system provides named sessions that group related projects and workflows into separate kitty OS windows. Each session contains multiple tabs focused on a specific type of development work.

## Available Sessions

### 1. Neovim Development (`nvim`)

**Keybinding:** `Ctrl+A 1`  
**Focus:** Neovim plugin development

- `nvim: Plugins` - ct.nvim development
- `nvim: QMK` - qmk.nvim plugin
- `nvim: Smart-Splits` - smart-splits.nvim plugin
- `nvim: Tests` - Test runner tab

### 2. Rust Development (`rust`)

**Keybing:** `Ctrl+A 2`  
**Focus:** Rust projects

- `rust: Pomo` - Pomo productivity app
- `rust: Barman` - Barman CLI tool
- `rust: Dotty` - Dotty file manager
- `rust: Build` - Cargo build/test commands

### 3. Web Development (`web`)

**Keybing:** `Ctrl+A 3`  
**Focus:** Web/JavaScript projects

- `web: Git Tools` - git-buddy project
- `web: Git Nudge` - git-nudge web app
- `web: PhotoSyncer` - PhotoSyncer React app
- `web: Terminal` - General terminal

### 4. Electron Development (`electron`)

**Keybing:** `Ctrl+A 4`  
**Focus:** Electron desktop applications

- `electron: Pancake` - pancake-electron app
- `electron: Pomo Electron` - pomo-electron app
- `electron: Dev Server` - Development server tab

### 5. QMK Development (`qmk`)

**Keybing:** `Ctrl+A 5`  
**Focus:** Keyboard firmware development

- `qmk: Firmware` - qmk_firmware main repository
- `qmk: Nuphy` - nuphy_firmware custom firmware
- `qmk: DAO Config` - dao-zmk-config keyboard layout
- `qmk: Compile` - Firmware compilation tab

## Key Features

### 🪟 Separate OS Windows

Each session creates its own OS window, allowing you to:

- Use Alt+Tab (or Cmd+Tab) to switch between sessions
- Have different sessions on different virtual desktops/spaces
- Work with multiple sessions simultaneously

### 🔍 Smart Session Detection

The system automatically detects existing sessions:

- **First use:** Creates new OS window with session tabs
- **Subsequent use:** Switches to existing session window
- **No duplicates:** Prevents creating multiple instances of the same session

### 🏷️ Prefixed Tab Names

All tabs are prefixed with the session name for easy identification:

```
nvim: Plugins    rust: Pomo       web: Git Tools
nvim: QMK        rust: Barman     web: Git Nudge
nvim: Tests      rust: Build      web: Terminal
```

## Usage

### Quick Access (Recommended)

Use tmux-style prefix mode:

1. Press `Ctrl+A` (enters prefix mode)
2. Press session number `1-5`
3. Session loads or switches if it exists

### Interactive Menu

For a visual menu of all sessions:

1. Press `Ctrl+A s`
2. Select from numbered list
3. Press Enter to load

### Command Line

Direct session loading:

```bash
# Create/switch to rust session
python3 ~/.config/kitty/load-session.py rust

# Interactive menu
python3 ~/.config/kitty/session-switcher.py
```

## Session Workflow

### Typical Usage Pattern

1. **Morning:** `Ctrl+A 1` - Load Neovim session for plugin work
2. **Mid-day:** `Ctrl+A 2` - Switch to Rust session for app development
3. **Afternoon:** `Ctrl+A 3` - Switch to Web session for frontend work
4. **Evening:** `Ctrl+A 5` - Switch to QMK session for keyboard tweaking

### Project Context Switching

Each session maintains its context:

- **Working directories** are set per tab
- **Editor sessions** remain open with file history
- **Terminal history** is preserved per tab
- **Build processes** can run in background tabs

## Technical Implementation

### File Structure

```
~/.config/kitty/
├── load-session.py        # Main session loader
├── session-switcher.py    # Interactive menu
├── sessions/              # Legacy session files (unused)
├── keys.conf             # Keybinding definitions
└── sessions.md           # This documentation
```

### Key Bindings in keys.conf

```conf
# Session management (tmux mode)
map --mode tmux s combine : launch --type=overlay python3 ~/.config/kitty/session-switcher.py : pop_keyboard_mode
map --mode tmux 1 combine : launch --hold python3 ~/.config/kitty/load-session.py nvim : pop_keyboard_mode
map --mode tmux 2 combine : launch --hold python3 ~/.config/kitty/load-session.py rust : pop_keyboard_mode
map --mode tmux 3 combine : launch --hold python3 ~/.config/kitty/load-session.py web : pop_keyboard_mode
map --mode tmux 4 combine : launch --hold python3 ~/.config/kitty/load-session.py electron : pop_keyboard_mode
map --mode tmux 5 combine : launch --hold python3 ~/.config/kitty/load-session.py qmk : pop_keyboard_mode
```

### Session Detection Logic

Sessions are identified by tab name prefixes:

- Searches all OS windows for tabs starting with `{session}:`
- If found, focuses that OS window
- If not found, creates new OS window with session tabs

## Customization

### Adding New Sessions

1. Edit `load-session.py`
2. Add new session function following the pattern:

```python
def load_my_session():
    """Load my custom session"""
    run_kitten_cmd("set-tab-title 'my: Main'")
    run_kitten_cmd("send-text 'cd ~/my/project ; clear ; nvim\\r'")
    # Add more tabs as needed
```

3. Add to session_map dictionary
4. Add keybinding in keys.conf

### Modifying Existing Sessions

Edit the corresponding `load_*_session()` function in `load-session.py`:

- Change working directories (`--cwd` or `cd` commands)
- Add/remove tabs
- Modify tab titles
- Change startup commands

### Custom Tab Layouts

Within each session function, you can:

- Use `--location=hsplit` or `--location=vsplit` for split layouts
- Set layouts with `goto-layout tall` or `goto-layout stack`
- Create complex multi-pane setups

## Troubleshooting

### Session Not Loading

1. Check if kitty remote control is enabled:
   ```bash
   grep "allow_remote_control" ~/.config/kitty/kitty.conf
   ```
2. Verify scripts are executable:
   ```bash
   ls -la ~/.config/kitty/*.py
   ```

### Duplicate Sessions Created

This usually means session detection failed. Check:

1. Tab naming consistency (must start with `{session}:`)
2. JSON parsing in `get_os_windows()` function
3. Run `kitten @ ls` manually to verify output format

### Keybindings Not Working

1. Verify tmux mode is active: `Ctrl+A` should show mode indicator
2. Check keys.conf syntax
3. Reload configuration: `Ctrl+A R`

## Comparison with tmux

| Feature             | tmux                        | Kitty Sessions         |
| ------------------- | --------------------------- | ---------------------- |
| Session isolation   | ✅ Process-based            | ✅ OS window-based     |
| Multiple sessions   | ✅ Named sessions           | ✅ Named sessions      |
| Session persistence | ✅ Survives disconnection   | ❌ Closed with kitty   |
| Tab management      | ✅ Windows & panes          | ✅ Tabs & windows      |
| Remote access       | ✅ Attach/detach            | ❌ Local only          |
| Resource usage      | Higher (separate processes) | Lower (single process) |
| Visual integration  | Terminal-based              | Native OS windows      |

## Benefits Over tmux

1. **Native OS integration** - Alt+Tab switching, proper window management
2. **Better performance** - Single process vs multiple tmux sessions
3. **Rich terminal features** - Full kitty graphics, ligatures, etc.
4. **Simpler setup** - No separate tmux configuration
5. **Visual clarity** - Proper window titles and tab labels

This session system provides the workflow benefits of tmux sessions while leveraging kitty's advanced terminal features and native OS integration.
