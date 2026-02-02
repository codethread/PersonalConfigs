---
description: Prompt claude to refine the current settings.json
disable-model-invocation: true
---

/docs please read about settings within claude code, and then modify the following files:

- claude/settings.json (this will be symlinked to `~/.claude/settings.json`)
- .claude/settings.json
- .claude/settings.local.json

Please compact the `allow` options together so the common options are available globally, locally (committed) and locally (not committed) as per the files above. ultrathink about the allow list to follow sensible groupings, and reasonable security practices.

For example i currently have `Bash(python3.11:*)`, which is a terrible idea as it allows anything, but my custom scripts like `Bash(cc-speak:*)` are fine, along with non destructive actions like `Bash(find:*)`

These files are json, so you can't use comments.
Local cli tools i have created (and can trust) are:
!`cindex --path home/.local/bin`
!`cindex --path oven/bin`
