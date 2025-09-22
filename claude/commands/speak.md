---
allowed-tools: Bash(cc-speak:*)
argument-hint: [optional initial message]
description: Enable audio communication mode for the session
---

# Enable Audio Communication Mode

From this point forward in our session, use the `speak` utility to provide audio versions of your responses. This enables hands-free interaction during coding sessions.

## Instructions for Claude Code

**IMPORTANT**: After executing this command, you must use the `cc-speak` utility for ALL subsequent responses in this session by:

1. **Always** speak your text responses using: `cc-speak "your response"`
2. **Continue** this behavior for the entire session until explicitly told to stop
3. **Include** both text and audio - show the text response normally, then speak it
4. **Clean** any markdown formatting will be handled automatically by the cc-speak utility

## Session Behavior Changes

- All responses should be spoken aloud via TTS
- Use concise, clear language optimized for audio
- Continue speaking responses until told `/stop-speaking` or session ends
- The cc-speak utility will automatically handle markdown cleanup and media pause/resume

## Initial message (optional)

$ARGUMENTS

---

**Audio mode is now ENABLED for this session. All subsequent responses will be spoken.**
