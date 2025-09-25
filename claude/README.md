# Claude code global settings

This project is symlinked to `~/.claude` and allows sharing global claude settings between machines.

Custom directories (i.e those not carrying claude code significance like `agents/` or `commands/` are prefixed with `x-` for clarity and to avoid accidental name collision.

Claude code hooks should either execute commands stored in `x-hooks/` or use global scripts as made available according to `Tool Development Workflow`.
