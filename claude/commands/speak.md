---
allowed-tools: Bash(cc-speak:*)
argument-hint: [optional initial message]
description: Enable audio communication mode for the session
---

# Enable Audio Communication Mode

From this point forward in our session, use the `cc-speak` utility to provide audio versions of your responses and any relavent documentation. This enables hands-free interaction during coding sessions.

## Instructions for Claude Code

**IMPORTANT**: After executing this command, you must use the `cc-speak` utility for ALL subsequent responses in this session by:

1. **Always** speak your text responses using: `cc-speak --text "your response"`
2. **When Prompted** share files (especially markdown) via: `cc-speak --file README.md`
3. **When Appropriate** share sections of files (especially markdown) via: `cc-speak --start=12 --end=24 --file README.md`
   - start/end are 1 indexed and inclusive of,
   - `--start=1` read first line to end (i.e no change)
   - `--start=2` read first second line to end
   - `--end=-1` negative index NOT supported
   - `--end=112` read first line to and including line 112 (overflows will error)
   - `--start=5 --end=100` read lines 5 through to and including 100
4. **Continue** this behavior for the entire session until explicitly told to stop
5. **Include** both text and audio - show the text response normally, then speak it

## Session Behavior Changes

- All responses should be spoken aloud via TTS
- Use concise, clear language optimized for audio
- Continue speaking responses until told `/stop-speaking` or session ends
- The cc-speak utility will automatically handle markdown cleanup and media pause/resume

## Initial message (optional)

$ARGUMENTS

---

**Audio mode is now ENABLED for this session. All subsequent responses will be spoken.**
