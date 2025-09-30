#!/usr/bin/env python3
"""
Kitty Watcher Script for Session Tracking

This watcher tracks directory-based sessions for custom session management.

## Session Tracking
Tracks tabs grouped by their current working directory (CWD) to create custom sessions.

### Session Structure
The sessions dict maps directories to tab dictionaries:
```
{
    '/user/projectA': { 12: true, 6: false, 222: false },
    '/user/projectB': { 8: true }
}
```

Where:
- Key: Current working directory path
- Value: Dict of tab_id -> is_focused_boolean
- `true`: Tab is currently focused within that session
- `false`: Tab exists in session but is not focused

### Behavior
- **New tab creation**: Detected via `on_focus_change` when tab_id not in known_tabs
  - Gets CWD from window and adds to appropriate session
  - Sets new tab as active (true), all others in session become inactive (false)
  - Sets SESSION user variable to kebab-case session name (e.g., "my-project")
  
- **Focus changes**: When switching to existing tab
  - Sets focused tab to true, all other tabs in same session to false
  - Only one tab per session can be active at a time
  
- **Tab closing**: Detected via `on_close` when last window in tab closes
  - Removes tab from its session
  - If last tab in session, removes entire session from dict

### User Variables
- **SESSION**: Set on each window to kebab-case version of directory name
  - `/home/user/my_project` -> `SESSION=my-project`  
  - `/home/user/MyApp` -> `SESSION=myapp`
  - Used via `kitten @ set-user-vars SESSION=session-name`

### Logging
- Regular events: ~/.config/kitty/watcher.log
- Session changes: ~/.config/kitty/sessions.log with actions:
  - `watcher_loaded`: Script initialization
  - `add_tab`: New tab added to session
  - `remove_tab`: Tab removed from session  
  - `remove_session`: Entire session removed (no tabs left)
  - `focus_change`: Active tab changed within session
- Errors: ~/.config/kitty/watcher_errors.log with full tracebacks

### Events Handled
- `on_focus_change`: Session tracking and user variable setting
- `on_close`: Session cleanup when tabs close
- `on_load`: Initialize session tracking

### Debugging
To debug session state from command line (non-TTY contexts):

#### Basic Session Inspection
```bash
# Find the kitty socket (has PID suffix)
ls -la /tmp/ | grep mykitty

# Use the socket to query kitty state
kitten @ --to unix:/tmp/mykitty-<PID> ls

# Example with dynamic socket discovery
KITTY_SOCKET=$(ls /tmp/mykitty-* 2>/dev/null | head -1)
kitten @ --to unix:$KITTY_SOCKET ls | jq '.[] | .tabs[] | .windows[] | .user_vars'
```

#### Testing with Fresh Kitty Instance
When debugging watcher issues, create a separate test instance to ensure the latest script is loaded:

```bash
# 1. Create test kitty instance with updated watcher
kitty --listen-on unix:/tmp/test-kitty-watcher --detach \
      --override allow_remote_control=yes \
      --override watcher=mywatcher.py &

# 2. Wait for startup then test tab creation
sleep 2
kitten @ --to unix:/tmp/test-kitty-watcher launch --type=tab --cwd=/path/to/test

# 3. Check SESSION variables and session tracking
kitten @ --to unix:/tmp/test-kitty-watcher ls | \
  jq '.[] | .tabs[] | .windows[] | {id: .id, cwd: .cwd, user_vars: .user_vars}'

# 4. Monitor detailed debug logs (enhanced logging)
tail -f ~/.config/kitty/sessions.log | \
  jq 'select(.action | contains("get_window_cwd") or contains("set_session_user_var"))'

# 5. Create tabs in different directories to test multiple sessions
kitten @ --to unix:/tmp/test-kitty-watcher launch --type=tab --cwd=/tmp
kitten @ --to unix:/tmp/test-kitty-watcher launch --type=tab --cwd=/Users/username

# 6. Verify session structure
tail -5 ~/.config/kitty/sessions.log | jq 'select(.action == "add_tab")'

# 7. Clean up test instance when done
kitten @ --to unix:/tmp/test-kitty-watcher close-window --match all
rm /tmp/test-kitty-watcher
```

#### Debug Log Analysis
The enhanced logging provides detailed information:
- `get_window_cwd_attempt`: CWD detection process and source
- `set_session_user_var_attempt`: User variable setting attempts
- `set_session_user_var_success`: Successful variable setting
- `set_session_user_var_failed`: Failed attempts with error details
```
"""

import json
import os
import re
from datetime import datetime
from typing import Any, Dict, Set

from kitty.boss import Boss
from kitty.window import Window

# Log file paths
LOG_FILE = os.path.expanduser("~/.config/kitty/watcher.log")
SESSION_LOG_FILE = os.path.expanduser("~/.config/kitty/sessions.log")
ERROR_LOG_FILE = os.path.expanduser("~/.config/kitty/watcher_errors.log")

# Global session tracking
sessions: Dict[str, Dict[int, bool]] = {}
known_tabs: Set[int] = set()


class SessionLogger:
    """Centralized logging for session tracking with multiple log types."""
    
    def __init__(self):
        self.event_log = LOG_FILE
        self.session_log = SESSION_LOG_FILE  
        self.error_log = ERROR_LOG_FILE
    
    def clear_logs(self) -> None:
        """Clear all log files for a fresh start."""
        for log_file in [self.event_log, self.session_log, self.error_log]:
            try:
                open(log_file, "w").close()
            except Exception as e:
                self._log_error("clear_logs", e)
    
    def window_info(self, window: Window) -> dict[str, Any]:
        """Extract standard window information."""
        return {
            "window_id": window.id,
            "title": window.title,
            "pid": window.child.pid if window.child else None,
            "tab_id": window.tabref().id if window.tabref() else None,
            "window_has_child": window.child is not None,
            "window_pid": window.child.pid if window.child else None
        }
    
    def event(self, event_name: str, window: Window, data: dict[str, Any]) -> None:
        """Log a kitty event."""
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "event": event_name,
            "window": {
                "id": window.id,
                "title": window.title,
                "pid": window.child.pid if window.child else None,
                "tab_id": window.tabref().id if window.tabref() else None,
            },
            "focused": data.get("focused", None)
        }
        self._write_log(self.event_log, log_entry)
    
    def session_change(self, action: str, cwd: str = "", tab_id: int = -1) -> None:
        """Log session state changes."""
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "action": action,
            "cwd": cwd,
            "tab_id": tab_id,
            "sessions": sessions.copy()
        }
        self._write_log(self.session_log, log_entry)
    
    def debug_attempt(self, action: str, window: Window, **extra_data) -> dict[str, Any]:
        """Log a debug attempt and return info for result logging."""
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "action": action,
            **self.window_info(window),
            **extra_data
        }
        self._write_log(self.session_log, log_entry)
        return log_entry
    
    def debug_result(self, debug_info: dict[str, Any], result_action: str, **result_data) -> None:
        """Log the result of a debug attempt."""
        result_info = debug_info.copy()
        result_info["action"] = result_action
        result_info.update(result_data)
        self._write_log(self.session_log, result_info)
    
    def error(self, context: str, error: Exception) -> None:
        """Log an error with full traceback."""
        import traceback
        
        error_entry = {
            "timestamp": datetime.now().isoformat(),
            "action": "error",
            "context": context,
            "error": str(error),
            "traceback": traceback.format_exc()
        }
        
        try:
            with open(self.error_log, "a") as f:
                f.write(json.dumps(error_entry) + "\n")
        except Exception:
            # Last resort: write to stderr
            import sys
            print(f"Critical error logging failure in {context}: {error}", file=sys.stderr)
    
    def _write_log(self, log_file: str, log_entry: dict[str, Any]) -> None:
        """Safely write a log entry to file."""
        try:
            with open(log_file, "a") as f:
                f.write(json.dumps(log_entry) + "\n")
        except Exception as e:
            self.error(f"write_log_{log_file}", e)
    
    def _log_error(self, context: str, error: Exception) -> None:
        """Internal error logging that doesn't create recursion."""
        import sys
        print(f"SessionLogger internal error in {context}: {error}", file=sys.stderr)


# Global logger instance
logger = SessionLogger()


# =============================================================================
# PUBLIC KITTY CALLBACK FUNCTIONS
# =============================================================================

def on_load(boss: Boss, data: dict[str, Any]) -> None:
    """Called once when this watcher module is first loaded."""
    # Clear the log files on load to start fresh
    logger.clear_logs()
    
    # Initialize sessions with existing tabs
    global sessions, known_tabs
    sessions.clear()
    known_tabs.clear()
    
    logger.session_change("watcher_loaded")


def on_focus_change(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    """Called when window focus changes."""
    try:
        logger.event("on_focus_change", window, data)
        
        # Only process when window gains focus
        if data.get("focused", False):
            # Session tracking
            if window.tabref():
                tab_id = window.tabref().id
                
                # Check if this is a new tab
                if tab_id not in known_tabs:
                    # This is a new tab, get its CWD and add to session
                    cwd = __get_window_cwd(window)
                    if cwd:
                        __add_tab_to_session(tab_id, cwd, window, boss)
                else:
                    # Existing tab gaining focus
                    __update_session_focus(tab_id)
    except Exception as e:
        logger.error("on_focus_change", e)


def on_close(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    """Called when window is closed."""
    try:
        logger.event("on_close", window, data)
        
        # Session tracking: check if this is the last window in its tab
        if window.tabref():
            tab_id = window.tabref().id
            tab = window.tabref()
            
            # Check if this is the last window in the tab
            if tab and len(tab.windows) <= 1:
                # This is the last (or only) window in the tab, so the tab will be closed
                __remove_tab_from_session(tab_id)
    except Exception as e:
        logger.error("on_close", e)


# =============================================================================
# PRIVATE HELPER FUNCTIONS (High to Low Level)
# =============================================================================

def __add_tab_to_session(tab_id: int, cwd: str, window: Window, boss: Boss) -> None:
    """Add a tab to a session based on its CWD."""
    if not cwd or tab_id in known_tabs:
        return
        
    # Add to known tabs
    known_tabs.add(tab_id)
    
    # Create session if it doesn't exist
    if cwd not in sessions:
        sessions[cwd] = {}
    
    # Set all other tabs in this session to inactive
    for tid in sessions[cwd]:
        sessions[cwd][tid] = False
    
    # Add this tab
    sessions[cwd][tab_id] = True
    
    # Set SESSION user variable on the window
    __set_session_user_var(boss, window, cwd)
    
    logger.session_change("add_tab", cwd, tab_id)


def __remove_tab_from_session(tab_id: int) -> None:
    """Remove a tab from its session."""
    if tab_id not in known_tabs:
        return
        
    # Find and remove the tab from sessions
    for cwd, tabs in list(sessions.items()):
        if tab_id in tabs:
            del tabs[tab_id]
            known_tabs.discard(tab_id)
            
            # If this was the last tab in the session, remove the session
            if not tabs:
                del sessions[cwd]
                logger.session_change("remove_session", cwd, tab_id)
            else:
                logger.session_change("remove_tab", cwd, tab_id)
            break


def __update_session_focus(tab_id: int) -> None:
    """Update which tab is active in its session."""
    for cwd, tabs in sessions.items():
        if tab_id in tabs:
            # Set all tabs in this session to inactive
            for tid in tabs:
                tabs[tid] = False
            # Set this tab as active
            tabs[tab_id] = True
            logger.session_change("focus_change", cwd, tab_id)
            break


def __set_session_user_var(boss: Boss, window: Window, cwd: str) -> None:
    """Set the SESSION user variable on the window."""
    session_name = __cwd_to_session_name(cwd)
    
    # Log the attempt with enhanced debugging info
    debug_info = logger.debug_attempt("set_session_user_var_attempt", window,
        cwd=cwd, 
        session_name=session_name
    )
    
    try:
        # Set the SESSION user variable on the window
        boss.call_remote_control(window, ('set-user-vars', f'SESSION={session_name}'))
        
        # Log success
        logger.debug_result(debug_info, "set_session_user_var_success")
            
    except Exception as e:
        # Log the error
        logger.debug_result(debug_info, "set_session_user_var_failed",
            error=str(e),
            error_type=type(e).__name__
        )
        logger.error("set_session_user_var", e)


def __get_window_cwd(window: Window) -> str:
    """Get the current working directory of a window."""
    try:
        if window.child and window.child.foreground_processes:
            # Get the CWD from the foreground process
            for proc in window.child.foreground_processes:
                if hasattr(proc, 'cwd') and proc.cwd:
                    logger.debug_attempt("get_window_cwd_attempt", window,
                        foreground_processes_count=len(window.child.foreground_processes),
                        cwd_source="foreground_process",
                        found_cwd=proc.cwd
                    )
                    return proc.cwd
        
        # Fallback: try to get from child process
        if window.child and hasattr(window.child, 'cwd'):
            child_cwd = getattr(window.child, 'cwd', '')
            if child_cwd:
                logger.debug_attempt("get_window_cwd_attempt", window,
                    foreground_processes_count=0,
                    cwd_source="child_process",
                    found_cwd=child_cwd
                )
                return child_cwd
            
        # Final fallback: use window's initial CWD if available
        window_cwd = getattr(window, 'cwd', '')
        logger.debug_attempt("get_window_cwd_attempt", window,
            foreground_processes_count=0,
            cwd_source="window_initial" if window_cwd else "none",
            found_cwd=window_cwd
        )
        return window_cwd
        
    except Exception as e:
        logger.debug_attempt("get_window_cwd_attempt", window,
            foreground_processes_count=0,
            cwd_source="error",
            found_cwd="",
            error=str(e)
        )
        logger.error("get_window_cwd", e)
        return ''


def __cwd_to_session_name(cwd: str) -> str:
    """Convert a CWD path to kebab-case session name."""
    if not cwd:
        return ''
    
    # Get the last directory name from the path
    session_name = os.path.basename(cwd.rstrip('/'))
    
    # Convert to kebab-case: replace spaces/underscores with hyphens, lowercase
    session_name = session_name.replace('_', '-').replace(' ', '-').lower()
    
    # Remove any non-alphanumeric characters except hyphens
    session_name = re.sub(r'[^a-z0-9-]', '', session_name)
    
    # Remove multiple consecutive hyphens
    session_name = re.sub(r'-+', '-', session_name)
    
    # Remove leading/trailing hyphens
    session_name = session_name.strip('-')
    
    return session_name or 'unknown'


