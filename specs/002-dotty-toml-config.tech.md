# Dotty TOML Configuration Migration - Technical Requirements

## Executive Summary

This technical specification outlines the implementation approach for migrating dotty's hardcoded Nushell configuration to an external TOML configuration file. The solution leverages Nushell's native TOML parsing capabilities, implements XDG Base Directory compliance, and maintains complete backward compatibility while enabling runtime configuration changes.

## Architecture Overview

### System Context

The dotty symlink management system will transition from embedded configuration to file-based configuration:

```
Current State:                          Target State:
┌──────────────────┐                   ┌──────────────────┐
│  config.nu       │                   │  config.nu       │
│  (hardcoded)     │  ──────────>      │  (TOML loader)   │
└──────────────────┘                   └────────┬─────────┘
                                                │ reads
                                        ┌───────▼─────────┐
                                        │ dotty.toml      │
                                        │ (~/.config)     │
                                        └─────────────────┘
```

### Component Architecture

1. **Configuration Loader** (`config/nushell/scripts/ct/dotty/config.nu`)
   - TOML file discovery and loading
   - Path expansion and normalization
   - Configuration validation
   - Error handling with clear messages

2. **Migration Module** (`config/nushell/scripts/ct/dotty/migrate.nu`)
   - Initial TOML file generation
   - Configuration directory setup
   - One-time migration from hardcoded values

3. **Validation Layer** (within `config.nu`)
   - Schema validation
   - Path verification
   - Duplicate detection
   - Type checking

### Data Architecture

Configuration data flow:

1. Check for TOML file at `$XDG_CONFIG_HOME/dotty/dotty.toml`
2. Parse TOML into Nushell table structure
3. Validate and expand paths
4. Transform to internal configuration format
5. Cache configuration for session (no changes to existing cache mechanism)

## Technology Decisions

### Framework/Library Selection

| Requirement    | Nushell Built-in                         | External Lib                | Recommendation                                     |
| -------------- | ---------------------------------------- | --------------------------- | -------------------------------------------------- |
| TOML Parsing   | Native `open` command, zero dependencies | Would require external tool | **Nushell built-in** - Already validated in spike  |
| Path Expansion | Native `path expand`                     | Custom implementation       | **Nushell native** - Consistent with existing code |
| File I/O       | Native file operations                   | N/A                         | **Nushell native** - Standard approach             |

### Integration Patterns

- **Configuration Loading**: Synchronous, fail-fast approach with clear error messages
- **Path Resolution**: Eager expansion at load time to catch errors early
- **Validation**: Multi-stage validation (syntax → schema → semantic)

## Implementation Tasks

### Component: Configuration Loader

Location: `config/nushell/scripts/ct/dotty/config.nu`

- [x] **CONFIG-1**: Create TOML configuration loader function (delivers FR-1) [TESTABLE]
  - Implement `load-config-from-toml` function
  - Check XDG_CONFIG_HOME and fallback to ~/.config
  - Handle file not found with clear error message
  - Updates: `config/nushell/scripts/ct/dotty/config.nu:1:1`

- [x] **CONFIG-2**: Implement path expansion for TOML values (delivers FR-4) [TESTABLE]
  - Expand tilde (~) in real and symlink paths
  - Support environment variable expansion
  - Validate expanded paths exist
  - Creates helper in: `config/nushell/scripts/ct/dotty/config.nu:50:1`

- [x] **CONFIG-3**: Add TOML schema validation (delivers FR-2, FR-5) [TESTABLE]
  - Validate required fields (name, real, symlink)
  - Check for duplicate project names
  - Validate excludes array format
  - Ensure link_directory is boolean
  - Creates validation in: `config/nushell/scripts/ct/dotty/config.nu:80:1`

- [x] **CONFIG-4**: Integrate TOML loader with existing config load function (delivers FR-6) [TESTABLE]
  - Modify existing `load` function to use TOML
  - Maintain same return format for compatibility
  - Remove hardcoded configuration
  - Updates: `config/nushell/scripts/ct/dotty/config.nu:15:1`

### Component: Migration Module

Location: `config/nushell/scripts/ct/dotty/migrate.nu`

- [x] **MIGRATE-1**: Create migration script for initial setup (delivers FR-3) [TESTABLE]
  - Extract current hardcoded configuration
  - Generate TOML structure with comments
  - Create configuration directory if missing
  - Creates: `config/nushell/scripts/ct/dotty/migrate.nu:1:1`

- [x] **MIGRATE-2**: Write TOML file with proper formatting (delivers FR-3) [TESTABLE]
  - Format TOML with sections and comments
  - Include migration timestamp
  - Preserve existing project settings exactly
  - Implements in: `config/nushell/scripts/ct/dotty/migrate.nu:30:1`

### Component: Error Handling [TEST AFTER COMPONENT]

Location: `config/nushell/scripts/ct/dotty/config.nu`

- [x] **ERROR-1**: Implement comprehensive error messages (delivers NFR-2)
  - Missing configuration file error
  - TOML syntax error with line number
  - Invalid path specifications
  - Missing required fields
  - Updates: `config/nushell/scripts/ct/dotty/config.nu:120:1`

- [x] **ERROR-2**: Add file permission error handling (delivers NFR-2)
  - Check read permissions on TOML file
  - Provide actionable error for permission issues
  - Updates: `config/nushell/scripts/ct/dotty/config.nu:140:1`

Note: Test ERROR-1 and ERROR-2 together as they form the complete error handling system

### Component: Performance Optimization

Location: `config/nushell/scripts/ct/dotty/config.nu`

- [x] **PERF-1**: Optimize configuration loading for sub-100ms performance (delivers NFR-1) [TESTABLE]
  - Load configuration once per session
  - Avoid redundant path expansions
  - Use efficient TOML parsing
  - Updates: `config/nushell/scripts/ct/dotty/config.nu:200:1`

## Technical Specifications

### Configuration File Schema

```toml
# Dotty configuration file
# Generated from hardcoded configuration on YYYY-MM-DD

[global]
# Global exclusion patterns applied to all projects
excludes = [
    "**/_?*/**",       # Files/folders starting with underscore
    "**/.gitignore",   # Git ignore files
    "**/README.md"     # README files
]

[[project]]
name = "home"
real = "~/PersonalConfigs/home"
symlink = "~"
excludes = []  # Additional project-specific excludes
link_directory = false  # Link individual files

[[project]]
name = "config"
real = "~/PersonalConfigs/config"
symlink = "~/.config"
excludes = []
link_directory = false

# Additional projects follow same structure...
```

### Error Message Specifications

| Error Type     | Message Format                                                                                       | Exit Code |
| -------------- | ---------------------------------------------------------------------------------------------------- | --------- |
| Missing Config | "Error: Configuration file not found at {path}\nRun 'dotty migrate' to create initial configuration" | 1         |
| TOML Syntax    | "Error: Invalid TOML syntax at line {line}: {detail}"                                                | 2         |
| Invalid Path   | "Error: Path '{path}' in project '{name}' does not exist"                                            | 3         |
| Duplicate Name | "Error: Duplicate project name '{name}' found in configuration"                                      | 4         |
| Permission     | "Error: Cannot read configuration file at {path}: Permission denied"                                 | 5         |

## Testing Strategy

### Unit Testing

Test coverage targets:

- TOML parsing: Valid and invalid configurations
- Path expansion: Tilde, environment variables, absolute paths
- Validation: All error conditions
- Migration: Correct TOML generation from hardcoded values

Mock strategy:

- Mock file system for path validation tests
- Use test TOML files in `tests/fixtures/`

### Integration Testing

Test scenarios:

1. Full migration from hardcoded to TOML configuration
2. All dotty commands with TOML configuration
3. Neovim integration with TOML config
4. Error handling for various failure modes

## Operational Considerations

### Logging

- Log configuration file path on load
- Log validation errors with context
- Debug mode to show expanded paths and final configuration

### Migration Communication

- Display clear message on first run after update
- Provide migration status and location of new config file
- Include rollback instructions if needed

## Technical Debt Considerations

Known limitations for initial implementation:

- No hot-reload of configuration (requires dotty restart)
- No configuration profiles support
- No include/inheritance mechanism
- Migration script can be removed after transition period

Future enhancements tracked in original specification.

## Dependencies and Prerequisites

- Nushell version 0.91.0+ (for stable TOML support)
- Write access to `$XDG_CONFIG_HOME` or `~/.config`
- Existing dotty installation with hardcoded configuration

## Regressions or Missed Requirements

None identified. All functional and non-functional requirements from the specification are addressed in the implementation tasks.
