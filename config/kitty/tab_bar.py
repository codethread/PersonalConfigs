"""Custom kitty tab bar with tmux-style layout.

This module provides a custom tab bar implementation for kitty terminal with:
- Session name displayed on the left (always in active colors, bold)
- Centered tabs with individual active/inactive styling
- Path shortening (e.g., ~/dev/foo/bar → bar)
- Stack layout indicator (e.g., +2 when multiple windows in stack)

## Implementation Details

The tab bar uses kitty's `tab_bar_style custom` mode, which calls `draw_tab()`
once for each tab. To achieve centered tabs:

1. On the first tab (index == 1): Reset global state and draw session name
2. For each tab: Accumulate tab data (title, colors, active state)
3. On the last tab (is_last == True): Draw all accumulated tabs centered

This accumulation pattern is necessary because kitty calls draw_tab() individually
for each tab, but we need to know all tabs to calculate the center position.

## Configuration

In kitty.conf:
    tab_bar_style custom
    tab_bar_min_tabs 1
    # Optional: filter to show only current session tabs
    tab_bar_filter session:~

## Debugging

To debug this tab bar with live output:

1. Run test kitty instance with stderr logging:
   ```bash
   kitty --config=/path/to/kitty.conf 2>/tmp/kitty-debug.log &
   KITTY_PID=$!
   sleep 2
   tail -f /tmp/kitty-debug.log  # Watch logs in real-time
   kill $KITTY_PID
   rm /tmp/kitty-debug.log
   ```

2. Add debug statements:
   ```python
   import sys
   print(f"DEBUG: message here", file=sys.stderr)
   ```

3. Inspect kitty state programmatically:
   ```bash
   # Find kitty socket
   KITTY_SOCKET=$(ls /tmp/mykitty-* 2>/dev/null | head -1)

   # List all tabs and windows with details
   kitten @ --to unix:$KITTY_SOCKET ls | jq

   # Check user variables
   kitten @ --to unix:$KITTY_SOCKET ls | jq '.[] | .tabs[] | .windows[] | .user_vars'
   ```

## Color API

Colors in kitty's tab bar must be converted:
- `draw_data.active_fg` returns a Color object
- Use: `as_rgb(color_as_int(draw_data.active_fg))` for screen.cursor.fg/bg

## Key Gotchas

- `draw_data.count` is a method, not a property (but len(draw_data) works)
- Global state (_tab_data) must be reset on first tab to handle tab changes
- Screen cursor position must be managed manually for centered layout
- Each tab needs individual color application to show active/inactive state
"""

from kitty.fast_data_types import Screen
from kitty.tab_bar import DrawData, ExtraData, TabBarData, as_rgb
from kitty.utils import color_as_int

# Global to accumulate tab data for centering
_tab_data = []


def _format_title(tab: TabBarData) -> str:
    """Format the tab title.

    Shows stack layout indicator when in stack mode.
    Formats paths to show only the last component (e.g., ~/dev/foo/bar → bar).
    """
    title = tab.title

    # Extract last path component if title looks like a path
    if '/' in title:
        title = title.rstrip('/').split('/')[-1]

    # Show stack indicator
    num_windows = getattr(tab, 'num_windows', 1)
    layout_name = getattr(tab, 'layout_name', '')

    if layout_name == 'stack' and num_windows > 1:
        return f" +{num_windows - 1} {title} "

    return f" {title} "


def draw_session_status(
    draw_data: DrawData,
    screen: Screen,
    tab: TabBarData,
    index: int
) -> None:
    """Draw session name at the start of the tab bar (only for first tab)."""
    if index != 1:
        return

    session_name = getattr(tab, 'session_name', '')
    if not session_name:
        return

    # Set active colors and bold for session name
    screen.cursor.fg = as_rgb(color_as_int(draw_data.active_fg))
    screen.cursor.bg = as_rgb(color_as_int(draw_data.active_bg))
    screen.cursor.bold = True

    # Draw session name
    screen.draw(f"   {session_name}  ")

    # Reset bold
    screen.cursor.bold = False


def draw_tab(
    draw_data: DrawData,
    screen: Screen,
    tab: TabBarData,
    before: int,
    max_title_length: int,
    index: int,
    is_last: bool,
    extra_data: ExtraData
) -> int:
    """Custom tab drawing function.

    Draws session name at start of tab bar (left), then centers all tabs.
    """
    global _tab_data

    # Reset tab data on first tab
    if index == 1:
        _tab_data = []
        # Draw session status at the left
        draw_session_status(draw_data, screen, tab, index)
        # Store session end position
        extra_data.session_end_x = screen.cursor.x

    # Format the tab title and store with styling info
    title = _format_title(tab)
    _tab_data.append({
        'title': title,
        'is_active': tab.is_active,
        'fg': draw_data.active_fg if tab.is_active else draw_data.inactive_fg,
        'bg': draw_data.active_bg if tab.is_active else draw_data.inactive_bg,
    })

    # On the last tab, draw all accumulated tabs centered
    if is_last:
        session_end_x = getattr(extra_data, 'session_end_x', 0)

        # Calculate total length of all tabs
        total_length = sum(len(t['title']) for t in _tab_data)

        # Calculate center position
        available_space = screen.columns - session_end_x
        center_x = session_end_x + (available_space // 2) - (total_length // 2)

        # Move cursor to center position
        screen.cursor.x = center_x

        # Draw each tab with its own styling
        for tab_info in _tab_data:
            screen.cursor.fg = as_rgb(color_as_int(tab_info['fg']))
            screen.cursor.bg = as_rgb(color_as_int(tab_info['bg']))
            screen.draw(tab_info['title'])

    return screen.cursor.x
