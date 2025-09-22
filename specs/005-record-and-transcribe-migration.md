# Record and Transcribe Migration

User Sign off: [APPROVED - Ready for implementation]

## Context and Problem Statement

Currently, `record-and-transcribe` exists as a bash script in `home/.local/bin/`. The script has a known issue with background process cleanup due to race conditions, leaving sox processes hanging. To align with the project's modern TypeScript/Bun toolchain and improve maintainability, this tool should be migrated to the oven workspace where it can benefit from TypeScript's type safety, better error handling, proper process lifecycle management, and integration with the existing build system.

## Value Statement

- **Consistency**: Aligns with the project's TypeScript/Bun standard for CLI tools
- **Maintainability**: TypeScript provides better IDE support, type safety, and refactoring capabilities
- **Testability**: Easier to unit test individual functions in TypeScript
- **Build Integration**: Leverages existing oven build pipeline and tooling
- **Bug Fix**: Resolves the existing race condition that leaves sox processes hanging

## Stakeholders

- **Users**: Developers using the record-and-transcribe tool for audio transcription workflows
- **Maintainers**: Project maintainers who manage the oven workspace
- **Dependencies**: sox, whisper-cli, whisper model files

## Technical Architecture

The tool will be reimplemented as a TypeScript CLI in `oven/bin/` following the existing patterns:

- Use Bun runtime for execution
- Maintain the same user interface (Space to stop and transcribe, Escape/Ctrl+C to cancel)
- Keep the same audio processing pipeline (sox → whisper)
- Implement robust process lifecycle management to prevent hanging processes

## Functional Requirements

### FR-1: Audio Recording

The system SHALL record audio from the default microphone using sox with:

- Sample rate: 16000 Hz
- Channels: 1 (mono)
- Bit depth: 16-bit
- Output format: WAV

### FR-2: Recording Control

The system SHALL:

- Start recording immediately when launched
- Display visual feedback during recording
- Stop recording when the Space key is pressed
- Stop recording and terminate the transcription process if the Escape key or Ctrl+C keys are hit.
- Provide confirmation when recording stops

### FR-3: Audio Transcription

The system SHALL:

- Transcribe recorded audio using whisper-cli
- Use the whisper model at `~/dev/models/ggml-medium.bin`
- Output transcribed text to stdout (unless recording was cancelled)
- Suppress whisper progress output

### FR-4: Dependency Checking

The system SHALL verify all required dependencies are installed:

- sox command availability
- whisper-cli command availability
- Whisper model file existence
- Provide helpful installation instructions if dependencies are missing

## Non-Functional Requirements

### NFR-1: Performance

- Transcription time SHALL match original whisper-cli performance

### NFR-2: Reliability

- Temporary audio files SHALL be cleaned up on exit (normal or interrupted)
- Terminal settings SHALL be restored after recording
- Process cleanup SHALL occur on all exit paths (SIGINT, SIGTERM, normal exit)
- All background processes (sox, whisper-cli) SHALL have their handles appropriately closed
- No orphaned processes SHALL remain after tool termination (fixes existing race condition bug)

### NFR-3: User Experience

- Clear visual indicators for recording state
- Colored output for better readability
- Informative error messages with remediation steps
- Silent operation except for status messages to stderr

## External Dependencies

- **sox**: Audio recording utility (installed via `brew install sox`)
- **whisper-cli**: Whisper transcription CLI (installed via `brew install whisper-cpp`)
- **Whisper Model**: GGML medium model file at `~/dev/models/ggml-medium.bin`

## Interface Definitions

### CLI Interface

```bash
# Usage
record-and-transcribe

# No arguments required
# Output: Transcribed text to stdout (only on successful recording)
# Status messages: stderr
# Exit codes:
#   0 - Success (recording completed and transcribed)
#   1 - Missing dependencies
#   2 - Recording error
#   3 - Transcription error
#   130 - User cancelled (Escape or Ctrl+C)
```

### User Controls

- **Space Bar**: Stop recording and proceed to transcription
- **Escape Key**: Cancel recording without transcription
- **Ctrl+C**: Cancel recording without transcription

## Acceptance Criteria

1. Tool records audio immediately when launched
2. Space bar stops recording and initiates transcription
3. Escape key or Ctrl+C cancels recording without transcription
4. Audio is transcribed and output to stdout only for successful recordings
5. All temporary files are cleaned up in all exit scenarios
6. Terminal state is properly restored in all exit scenarios
7. No background processes (sox, whisper) remain after tool exits
8. Dependency checks provide helpful installation guidance
9. Tool builds successfully with `bun run build`
10. Executable works from `~/.local/bin/` after build
11. Exit codes correctly indicate success (0) or cancellation (130)

## Implementation Notes

- Reuse patterns from existing oven tools (gmr, bra, notif)
- Use Node.js readline or similar for keyboard input handling
- Spawn sox as a child process and carefully manage its lifecycle
- Terminal raw mode handling will be critical for Space/Escape key detection
- Implement proper signal handlers to ensure process cleanup on all exit paths
- Use try/finally blocks to guarantee cleanup even on unexpected errors
- Track all spawned process PIDs for reliable cleanup
- Consider using process groups for more robust subprocess management

## Technical Debt Tracking

- Future: Consider adding support for different whisper models via CLI flags
- Future: Add option to save audio files instead of auto-deleting
- Future: Support for custom sample rates and audio formats
- Future: Progress indicator during transcription for long recordings
